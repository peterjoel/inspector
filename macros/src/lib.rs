use darling::{
    ast::{Data, Fields, Style},
    Error, FromDeriveInput, Result,
};
use proc_macro2::{Literal, Span, TokenStream};
use quote::{quote, ToTokens};
use std::iter::Extend;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, Field, Generics, Ident,
    Lifetime, TypeGenerics, Variant, WhereClause, WherePredicate,
};

#[derive(FromDeriveInput)]
#[darling(attributes(clouseau))]
pub(crate) struct ClouseauDeriveInput {
    #[darling(default, rename = "value")]
    value_convert: bool,
    #[darling(default)]
    transparent: bool,
    generics: Generics,
    ident: Ident,
    data: Data<Variant, Field>,
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
    let name = &input.ident;
    let q_life = &Lifetime::new("'__clouseau_q", Span::call_site());
    let a_life = &Lifetime::new("'__clouseau_a", Span::call_site());
    let (impl_generics, type_generics, where_clause) =
        create_generics(q_life.clone(), &input.generics);
    let (member_body, all_body, data_body, keys_body, value_impls) = match &input.data {
        Data::Struct(data) => (
            struct_member_body(data, input.transparent)?,
            struct_all_body(data)?,
            struct_data_body(data, input.transparent)?,
            struct_keys_body(data)?,
            impl_to_from_value(name, data, input.value_convert)?,
        ),
        Data::Enum(data) => (
            enum_member_body(data)?,
            enum_all_body(data)?,
            enum_data_body(data)?,
            enum_keys_body(data)?,
            TokenStream::new(),
        ),
    };
    Ok(quote! {
        impl#impl_generics ::clouseau::core::Queryable<#q_life> for #name#type_generics #where_clause {
            fn name(&self) -> &'static str {
                stringify!(#name)
            }
            fn keys(&self) -> clouseau::core::ValueIter<'_> {
                #keys_body
            }
            fn member<#a_life, 'f>(&#a_life self, field_name: &'f ::clouseau::core::Value) ->
                Option<::clouseau::core::Node<#a_life, #q_life>>
            {
                #member_body
            }
            fn all<#a_life>(&#a_life self) -> ::clouseau::core::NodeOrValueIter<#a_life, #q_life> {
                #all_body
            }
            fn data(&self) -> Option<::clouseau::core::Value> {
                #data_body
            }
        }
        #value_impls
    })
}

struct ImplGenerics<'a>(&'a Generics, Lifetime);

impl<'a> ToTokens for ImplGenerics<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let lifetime = &self.1;
        let original_generics = self.0.params.iter();
        tokens.extend(quote! {
            <#lifetime, #(#original_generics),*>
        });
    }
}

fn create_generics(
    q_life: Lifetime,
    generics: &Generics,
) -> (ImplGenerics<'_>, TypeGenerics<'_>, WhereClause) {
    let where_clause = where_clause_with_bound(
        generics,
        quote! {
            ::clouseau::core::Queryable<#q_life>
        },
    );
    let (_, type_generics, _) = generics.split_for_impl();
    (ImplGenerics(generics, q_life), type_generics, where_clause)
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

fn struct_member_body(data: &Fields<Field>, transparent: bool) -> Result<TokenStream> {
    match data.style {
        Style::Tuple => {
            if data.fields.len() == 1 && transparent {
                Ok(quote! { self.0.member(field_name) })
            } else {
                let indices = (0..data.fields.len() as i64).map(Literal::i64_unsuffixed);
                let indices2 = (0..data.fields.len()).map(Literal::usize_unsuffixed);
                Ok(quote! {
                    match field_name {
                        #(
                            ::clouseau::core::Value::Int(#indices) => { Some(&self.#indices as _) },
                        )*
                        ::clouseau::core::Value::String(s) => match s.parse::<usize>() {
                            #(Ok(#indices2) => Some(&self.#indices2 as _),)*
                            _ => None,
                        }
                        _ => None,
                    }
                })
            }
        }
        Style::Struct => {
            if data.fields.len() == 1 && transparent {
                let name = &data.fields[0].ident;
                Ok(quote! { self.#name.member(field_name) })
            } else {
                let names = data.fields.iter().filter_map(|named| named.ident.as_ref());
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
        Style::Unit => Ok(TokenStream::new()),
    }
}

fn struct_all_body(data: &Fields<Field>) -> Result<TokenStream> {
    Ok(match data.style {
        Style::Tuple => {
            let indices = (0..data.fields.len() as i64).map(Literal::i64_unsuffixed);
            quote! {
                ::clouseau::core::NodeOrValueIter::from_nodes(vec![#(&self.#indices as _),*])
            }
        }
        Style::Struct => {
            let names = data.fields.iter().filter_map(|named| named.ident.as_ref());
            quote! {
                ::clouseau::core::NodeOrValueIter::from_nodes(vec![#(&self.#names as _),*])
            }
        }
        Style::Unit => quote! {
            ::clouseau::core::NodeOrValueIter::empty()
        },
    })
}

fn struct_keys_body(data: &Fields<Field>) -> Result<TokenStream> {
    Ok(match data.style {
        Style::Tuple => {
            let len = data.fields.len();
            quote! {
                ::clouseau::core::ValueIter::from_values(0..#len)
            }
        }
        Style::Struct => {
            let names = data.fields.iter().filter_map(|named| named.ident.as_ref());
            quote! {
                ::clouseau::core::ValueIter::from_values(vec![#(stringify!(#names)),*])
            }
        }
        Style::Unit => quote! {
            ::clouseau::core::ValueIter::empty()
        },
    })
}

fn struct_data_body(data: &Fields<Field>, transparent: bool) -> Result<TokenStream> {
    match data.style {
        Style::Tuple | Style::Struct if data.fields.len() == 1 && transparent => {
            Ok(quote! { Some(std::clone::Clone::clone(self).into()) })
        }
        _ => {
            if transparent {
                Err(Error::unsupported_shape(
                    "Only structs with a single field may be atomic",
                ))
            } else {
                Ok(quote! { None })
            }
        }
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
    data: &Fields<Field>,
    value_convert: bool,
) -> Result<TokenStream> {
    // TODO support generics
    match data.style {
        Style::Struct if value_convert && data.fields.len() == 1 => {
            let ty = &data.fields[0].ty;
            let ident = &data.fields[0].ident;
            Ok(quote! {
                impl std::convert::TryFrom<::clouseau::core::Value> for #name {
                    type Error = <#ty as std::convert::TryFrom<::clouseau::core::Value>>::Error;
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
            })
        }
        Style::Tuple if value_convert && data.fields.len() == 1 => {
            let ty = &data.fields[0].ty;
            Ok(quote! {
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
            })
        }
        _ if value_convert => Err(Error::unsupported_shape(
            "Value conversions may only be implemented for structs with a single field",
        )),
        _ => Ok(TokenStream::new()),
    }
}
