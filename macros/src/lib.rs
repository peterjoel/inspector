use darling::{
    ast::{self, Data, Style},
    Error, FromDeriveInput, FromField, Result,
};
use itertools::Either::*;
use proc_macro2::{Literal, TokenStream};
use quote::{quote, ToTokens};
use std::iter::{self, Extend};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, Generics, Ident, Path,
    TypeGenerics, Variant, WhereClause, WherePredicate,
};

#[derive(FromDeriveInput)]
#[darling(attributes(clouseau))]
struct ClouseauDeriveInput {
    #[darling(default, rename = "value")]
    is_value: bool,
    #[darling(default, rename = "as")]
    value_as: Option<Path>,
    #[darling(default)]
    transparent: bool,
    generics: Generics,
    ident: Ident,
    data: Data<Variant, Field>,
}

#[derive(Debug, FromField)]
#[darling(attributes(clouseau))]
struct Field {
    #[darling(default)]
    skip: bool,
    ident: Option<syn::Ident>,
    ty: syn::Type,
}

#[derive(Copy, Clone, Debug)]
struct StructFields<'f>(&'f ast::Fields<Field>);

#[derive(Copy, Clone, Debug)]
struct TupleFields<'f>(&'f ast::Fields<Field>);

#[derive(Copy, Clone, Debug)]
enum Fields<'f> {
    Tuple(TupleFields<'f>),
    Struct(StructFields<'f>),
    Unit,
}

impl<'f> From<&'f ast::Fields<Field>> for Fields<'f> {
    fn from(fields: &'f ast::Fields<Field>) -> Self {
        match fields.style {
            Style::Tuple => Fields::Tuple(TupleFields(fields)),
            Style::Struct => Fields::Struct(StructFields(fields)),
            Style::Unit => Fields::Unit,
        }
    }
}

impl<'f> Fields<'f> {
    fn len(&self) -> usize {
        match self {
            Fields::Tuple(fields) => fields.len(),
            Fields::Struct(fields) => fields.len(),
            Fields::Unit => 0,
        }
    }
}

impl<'f> StructFields<'f> {
    fn fields(&self) -> impl Iterator<Item = &'f Field> + 'f {
        match self.0 {
            ast::Fields {
                style: Style::Struct,
                fields,
                ..
            } => Left(fields.iter().filter(|field| !field.skip)),
            _ => Right(iter::empty()),
        }
    }

    fn names(&self) -> impl Iterator<Item = &'f syn::Ident> + 'f {
        self.fields().filter_map(|field| field.ident.as_ref())
    }

    fn len(&self) -> usize {
        self.fields().count()
    }
}

impl<'f> TupleFields<'f> {
    fn indexed_fields(&self) -> impl Iterator<Item = (usize, &'f Field)> + 'f {
        match &self.0 {
            ast::Fields {
                style: Style::Tuple,
                fields,
                ..
            } => Left(fields.iter().enumerate().filter(|(_, field)| !field.skip)),
            _ => Right(iter::empty()),
        }
    }

    fn indices(&self) -> impl Iterator<Item = usize> + 'f {
        self.indexed_fields().map(|(index, _)| index)
    }

    fn fields(&self) -> impl Iterator<Item = &'f Field> + 'f {
        self.indexed_fields().map(|(_, field)| field)
    }

    fn len(&self) -> usize {
        self.indexed_fields().count()
    }
}

#[proc_macro_derive(Queryable, attributes(clouseau))]
pub fn derive_queryable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    match impl_queryable(FromDeriveInput::from_derive_input(&derive_input).unwrap()) {
        Ok(tokens) => tokens,
        Err(e) => e.write_errors(),
    }
    .into()
}

fn impl_queryable(input: ClouseauDeriveInput) -> Result<TokenStream> {
    if input.transparent && input.value_as.is_some() {
        return Err(Error::custom(
            "`transparent` and `value` may not be specified together",
        ));
    }
    let name = &input.ident;
    let (impl_generics, type_generics, where_clause) = create_generics(&input.generics);
    let (member_body, all_body, data_body, keys_body, value_impls) = match &input.data {
        Data::Struct(data) => {
            let fields = data.into();
            (
                struct_member_body(fields, input.transparent)?,
                struct_all_body(fields, input.transparent)?,
                struct_data_body(fields, input.transparent, input.is_value)?,
                struct_keys_body(fields, input.transparent)?,
                impl_to_from_value(name, fields, input.is_value, input.value_as.as_ref())?,
            )
        }
        Data::Enum(data) => {
            if input.transparent {
                return Err(Error::custom("`transparent` is not supported in enums"));
            } else {
                (
                    enum_member_body(data)?,
                    enum_all_body(data)?,
                    enum_data_body(data)?,
                    enum_keys_body(data)?,
                    TokenStream::new(),
                )
            }
        }
    };
    Ok(quote! {
        impl#impl_generics ::clouseau::core::Queryable for #name#type_generics #where_clause {
            fn name(&self) -> &'static str {
                stringify!(#name)
            }
            fn keys(&self) -> clouseau::core::ValueIter<'_> {
                #keys_body
            }
            fn member<'f>(&self, field_name: &'f ::clouseau::core::Value) -> Option<::clouseau::core::Node<'_>> {
                #member_body
            }
            fn all(&self) -> ::clouseau::core::NodeOrValueIter<'_> {
                #all_body
            }
            fn data(&self) -> Option<::clouseau::core::Value> {
                #data_body
            }
        }
        #value_impls
    })
}

struct ImplGenerics<'a>(&'a Generics);

impl<'a> ToTokens for ImplGenerics<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let original_generics = self.0.params.iter();
        tokens.extend(quote! {
            <#(#original_generics),*>
        });
    }
}

fn create_generics(generics: &Generics) -> (ImplGenerics<'_>, TypeGenerics<'_>, WhereClause) {
    let where_clause = where_clause_with_bound(
        generics,
        quote! {
            ::clouseau::core::Queryable
        },
    );
    let (_, type_generics, _) = generics.split_for_impl();
    (ImplGenerics(generics), type_generics, where_clause)
}

fn where_clause_with_bound(generics: &Generics, bound: TokenStream) -> WhereClause {
    let new_predicates = generics.type_params().map::<WherePredicate, _>(|param| {
        let param = &param.ident;
        parse_quote!(#param : #bound)
    });

    let mut generics = generics.clone();
    generics
        .make_where_clause()
        .predicates
        .extend(new_predicates);
    generics.where_clause.unwrap()
}

fn struct_member_body(data: Fields, transparent: bool) -> Result<TokenStream> {
    match data {
        Fields::Tuple(fields) => {
            if fields.len() == 1 && transparent {
                Ok(quote! { self.0.member(field_name) })
            } else {
                let indices_int = fields.indices().map(|i| Literal::i64_unsuffixed(i as i64));
                let indices_str = fields.indices().map(Literal::usize_unsuffixed);
                Ok(quote! {
                    match field_name {
                        #(
                            ::clouseau::core::Value::Int(#indices_int) => { Some(&self.#indices_int as _) },
                        )*
                        ::clouseau::core::Value::String(s) => match s.parse::<usize>() {
                            #(Ok(#indices_str) => Some(&self.#indices_str as _),)*
                            _ => None,
                        }
                        _ => None,
                    }
                })
            }
        }
        Fields::Struct(fields) => {
            if fields.len() == 1 && transparent {
                let name = fields.names().next().unwrap();
                Ok(quote! { self.#name.member(field_name) })
            } else {
                let names = fields.names();
                Ok(quote! {
                    match field_name {
                        ::clouseau::core::Value::String(s) => {
                            match s.as_str() {
                                #(stringify!(#names) => Some(&self.#names as _),)*
                                _ => None,
                            }
                        }
                        _ => None,
                    }
                })
            }
        }
        Fields::Unit => {
            if transparent {
                Err(Error::custom(
                    "transparent is only supported in structs with one field",
                ))
            } else {
                Ok(TokenStream::new())
            }
        }
    }
}

fn struct_all_body(data: Fields, transparent: bool) -> Result<TokenStream> {
    match data {
        Fields::Tuple(fields) => {
            if transparent {
                if fields.len() == 1 {
                    Ok(quote! {
                        self.0.all()
                    })
                } else {
                    Err(Error::unsupported_shape(
                        "transparent may only be used with structs with one field",
                    ))
                }
            } else {
                let indices = fields
                    .indices()
                    .map(|index| Literal::i64_unsuffixed(index as i64));
                Ok(quote! {
                    ::clouseau::core::NodeOrValueIter::from_nodes(vec![#(&self.#indices as _),*])
                })
            }
        }
        Fields::Struct(fields) => {
            if transparent {
                if fields.len() == 1 {
                    let name = fields.names().next().unwrap();
                    Ok(quote! {
                        self.#name.all()
                    })
                } else {
                    Err(Error::unsupported_shape(
                        "transparent may only be used with structs with one field",
                    ))
                }
            } else {
                let names = fields.names();
                Ok(quote! {
                    ::clouseau::core::NodeOrValueIter::from_nodes(vec![#(&self.#names as _),*])
                })
            }
        }
        Fields::Unit => {
            if transparent {
                Err(Error::custom(
                    "transparent is only supported in structs with one field",
                ))
            } else {
                Ok(quote! {
                    ::clouseau::core::NodeOrValueIter::empty()
                })
            }
        }
    }
}

fn struct_keys_body(data: Fields, transparent: bool) -> Result<TokenStream> {
    match data {
        Fields::Tuple(fields) => {
            if transparent {
                if fields.len() == 1 {
                    Ok(quote! {
                        self.0.keys()
                    })
                } else {
                    Err(Error::custom(
                        "transparent is only supported in structs with one field",
                    ))
                }
            } else {
                let indices = fields.indices();
                Ok(quote! {
                    ::clouseau::core::ValueIter::from_values(vec![#(#indices),*])
                })
            }
        }
        Fields::Struct(fields) => {
            if transparent {
                if fields.len() == 1 {
                    let name = fields.names().next().unwrap();
                    Ok(quote! {
                        self.#name.keys()
                    })
                } else {
                    Err(Error::custom(
                        "transparent is only supported in structs with one field",
                    ))
                }
            } else {
                let names = fields.names();
                Ok(quote! {
                    ::clouseau::core::ValueIter::from_values(vec![#(stringify!(#names)),*])
                })
            }
        }
        Fields::Unit => {
            if transparent {
                Err(Error::custom(
                    "transparent is only supported in structs with one field",
                ))
            } else {
                Ok(quote! {
                    ::clouseau::core::ValueIter::empty()
                })
            }
        }
    }
}

fn struct_data_body(data: Fields, transparent: bool, is_value: bool) -> Result<TokenStream> {
    if is_value {
        return Ok(quote! { Some(Value::from(self.clone()))});
    }

    if data.len() == 1 && transparent {
        match data {
            Fields::Tuple(fields) => {
                let index = Literal::usize_unsuffixed(fields.indices().next().unwrap());
                Ok(quote! { self.#index.data() })
            }
            Fields::Struct(fields) => {
                let name = fields.names().next().unwrap();
                Ok(quote! { self.#name.data() })
            }
            Fields::Unit => unreachable!(),
        }
    } else if transparent {
        Err(Error::unsupported_shape(
            "Only structs with a single field may be transparent",
        ))
    } else {
        Ok(quote! { None })
    }
}

fn enum_member_body(data: &[Variant]) -> Result<TokenStream> {
    let variants = data.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            syn::Fields::Unnamed(fields) => {
                let bindings = (0..fields.unnamed.len())
                    .map(|i| Ident::new(&format!("f{}", i), fields.span()));
                let bindings2 = bindings.clone();
                let bindings3 = bindings.clone();
                let indices = (0..fields.unnamed.len() as i64).map(Literal::i64_unsuffixed);
                let indices2 = indices.clone();
                quote! {
                    Self::#name(#(#bindings),*) => {
                        match field_name {
                            #(
                                ::clouseau::core::Value::Int(#indices) => { Some(#bindings2 as _) },
                            )*
                            ::clouseau::core::Value::String(s) => if let Ok(i) = s.parse::<i64>() {
                                match i {
                                    #(#indices2 => Some(#bindings3 as _),)*
                                    _ => None,
                                }
                            } else { None },
                            _ => None,
                        }
                    }
                }
            }
            syn::Fields::Named(fields) => {
                let bindings = fields.named.iter().map(|field| &field.ident);
                let bindings2 = bindings.clone();
                quote! {
                    Self::#name { #(#bindings),* } => {
                        match field_name {
                            ::clouseau::core::Value::String(s) => match s.as_str() {
                                #(stringify!(#bindings2) => { Some(#bindings2 as _) },)*
                                _ => None,
                            }
                            _ => None,
                        }
                    }
                }
            }
            syn::Fields::Unit => {
                quote! {
                    Self::#name => None
                }
            }
        }
    });
    Ok(quote! {
        match self {
            #(#variants,)*
        }
    })
}

fn enum_keys_body(data: &[Variant]) -> Result<TokenStream> {
    let variants = data.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            syn::Fields::Unnamed(fields) => {
                let len = fields.unnamed.len();
                quote! {
                    Self::#name(..) => ::clouseau::core::ValueIter::from_values(0..#len)
                }
            }
            syn::Fields::Named(fields) => {
                let names = fields.named.iter().map(|field| field.ident.as_ref().unwrap().to_string());
                quote! {
                    Self::#name { .. } => ::clouseau::core::ValueIter::from_values(vec![#(#names),*])
                }
            }
            syn::Fields::Unit => {
                quote! {
                    Self::#name => ::clouseau::core::ValueIter::empty()
                }
            }
        }
    });
    Ok(quote! {
        match self {
            #(#variants,)*
        }
    })
}

fn enum_all_body(data: &[Variant]) -> Result<TokenStream> {
    let variants = data.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            syn::Fields::Unnamed(fields) => {
                let bindings = (0..fields.unnamed.len())
                    .map(|i| Ident::new(&format!("f{}", i), fields.span()));
                let bindings2 = bindings.clone();
                quote! {
                    Self::#name(#(#bindings),*) => ::clouseau::core::NodeOrValueIter::from_nodes(vec![#(#bindings2 as _,)*])
                }
            }
            syn::Fields::Named(fields) => {
                let bindings = fields.named.iter().map(|field| &field.ident);
                let bindings2 = bindings.clone();
                quote! {
                    Self::#name { #(#bindings),* } => ::clouseau::core::NodeOrValueIter::from_nodes(vec![#(#bindings2 as _,)*])
                }
            }
            syn::Fields::Unit => {
                quote! {
                    Self::#name => ::clouseau::core::NodeOrValueIter::empty()
                }
            }
        }
    });
    Ok(quote! {
        match self {
            #(#variants,)*
        }
    })
}

fn enum_data_body(data: &[Variant]) -> Result<TokenStream> {
    let variants = data.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            syn::Fields::Unnamed(_) => {
                quote! {
                    Self::#name(..) => Some(stringify!(#name).to_string().into())
                }
            }
            syn::Fields::Named(_) => {
                quote! {
                    Self::#name { .. } => Some(stringify!(#name).to_string().into())
                }
            }
            syn::Fields::Unit => {
                quote! {
                    Self::#name => Some(stringify!(#name).to_string().into())
                }
            }
        }
    });
    Ok(quote! {
        match self {
            #(
                #variants,
            )*
        }
    })
}

fn impl_to_from_value(
    name: &Ident,
    data: Fields,
    is_value: bool,
    value_as: Option<&Path>,
) -> Result<TokenStream> {
    // TODO support generics
    if is_value {
        if let Some(as_type) = value_as.as_ref() {
            return Ok(quote! {
                impl std::convert::TryFrom<::clouseau::core::Value> for #name {
                    type Error = clouseau::core::Error;
                    fn try_from(value: clouseau::core::Value) -> std::result::Result<Self, Self::Error> {
                        let via: #as_type = std::convert::TryInto::try_into(value)?;
                        via.try_into().map_err(|e| clouseau::core::Error::Conversion(Box::from(e)))
                    }
                }
                impl std::convert::From<#name> for ::clouseau::core::Value {
                    fn from(other: #name) -> Self {
                        let via: #as_type = other.into();
                        Self::from(via)
                    }
                }
            });
        } else {
            match data {
                Fields::Struct(fields) if fields.len() == 1 => {
                    let field = fields.fields().next().unwrap();
                    let ty = &field.ty;
                    let ident = field.ident.as_ref().unwrap();
                    return Ok(quote! {
                        impl std::convert::TryFrom<::clouseau::core::Value> for #name {
                            type Error = clouseau::core::Error;
                            fn try_from(value: clouseau::core::Value) -> std::result::Result<Self, Self::Error> {
                                let inner: #ty = std::convert::TryInto::try_into(value)?;
                                Ok(#name { #ident: inner })
                            }
                        }
                        impl std::convert::From<#name> for ::clouseau::core::Value {
                            fn from(other: #name) -> Self {
                                Self::from(other.#ident)
                            }
                        }
                    });
                }
                Fields::Tuple(fields) if fields.len() == 1 => {
                    let ty = &fields.fields().next().unwrap().ty;
                    return Ok(quote! {
                        impl std::convert::TryFrom<::clouseau::core::Value> for #name {
                            type Error = <#ty as std::convert::TryFrom<::clouseau::core::Value>>::Error;
                            fn try_from(value: ::clouseau::core::Value) -> std::result::Result<Self, Self::Error> {
                                let inner: #ty = std::convert::TryInto::try_into(value)?;
                                Ok(#name(inner))
                            }
                        }
                        impl std::convert::From<#name> for ::clouseau::core::Value {
                            fn from(other: #name) -> Self {
                                Self::from(other.0)
                            }
                        }
                    });
                }
                _ => {
                    return Err(Error::unsupported_shape(
                        "Value conversions may only be implemented for structs with a single field",
                    ))
                }
            }
        }
    }

    Ok(TokenStream::new())
}
