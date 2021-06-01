use proc_macro2::{Literal, Span, TokenStream};
use quote::{quote, ToTokens};
use std::iter::Extend;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput,
    Fields, Generics, Ident, Lifetime, TypeGenerics, WhereClause, WherePredicate,
};

#[proc_macro_derive(Queryable)]
pub fn derive_queryable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impl_queryable(parse_macro_input!(input as DeriveInput)).into()
}

fn impl_queryable(input: DeriveInput) -> TokenStream {
    let name = &input.ident;
    let q_life = &Lifetime::new("'__clouseau_a", Span::call_site());
    let (impl_generics, type_generics, where_clause) =
        create_generics(q_life.clone(), &input.generics);
    let (member_body, all_body, data_body) = match &input.data {
        Data::Struct(data) => (
            struct_member_body(data),
            struct_all_body(data),
            struct_data_body(data),
        ),
        Data::Enum(data) => (
            enum_member_body(data),
            enum_all_body(data),
            enum_data_body(data),
        ),
        Data::Union(_) => return quote! { compile_error!("Unions are not supported"); },
    };
    quote! {
        impl#impl_generics ::clouseau::core::Queryable<#q_life> for #name#type_generics #where_clause {
            fn name(&self) -> &'static str {
                stringify!(#name)
            }
            fn member<'f>(&#q_life self, field_name: &'f ::clouseau::core::Value) -> Option<&#q_life dyn ::clouseau::core::Queryable<#q_life>> {
                #member_body
            }
            fn all(&#q_life self) -> ::clouseau::core::TreeIter<#q_life> {
                ::clouseau::core::TreeIter(#all_body)
            }
            fn data(&self) -> Option<::clouseau::core::Value> {
                #data_body
            }
        }
    }
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
    clouseau_lifetime: Lifetime,
    generics: &Generics,
) -> (ImplGenerics<'_>, TypeGenerics<'_>, WhereClause) {
    let lifetime = &clouseau_lifetime;
    let where_clause = where_clause_with_bound(
        generics,
        quote! {
            ::clouseau::core::Queryable<#lifetime>
        },
    );
    let (_, type_generics, _) = generics.split_for_impl();
    (
        ImplGenerics(generics, clouseau_lifetime),
        type_generics,
        where_clause,
    )
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

fn struct_member_body(data: &DataStruct) -> TokenStream {
    match &data.fields {
        Fields::Unnamed(fields) => {
            // Special case for newtypes: just delegate to inner type
            // TODO: review is this ok or not - probably should be opt-in?
            if fields.unnamed.len() == 1 {
                quote! { self.0.member(field_name) }
            } else {
                let indices = (0..fields.unnamed.len() as i64).map(|i| Literal::i64_unsuffixed(i));
                let indices2 = (0..fields.unnamed.len()).map(|i| Literal::usize_unsuffixed(i));
                quote! {
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
                }
            }
        }
        Fields::Named(fields) => {
            let names = fields.named.iter().filter_map(|named| named.ident.as_ref());
            quote! {
                match field_name {
                    ::clouseau::core::Value::String(s) => {
                        match s.as_str() {
                            #(stringify!(#names) => Some(&self.#names as _),)*
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
        }
        Fields::Unit => TokenStream::new(),
    }
}

fn struct_all_body(data: &DataStruct) -> TokenStream {
    match &data.fields {
        Fields::Unnamed(fields) => {
            let indices = (0..fields.unnamed.len() as i64).map(|i| Literal::i64_unsuffixed(i));
            quote! {
                Box::from(vec![#(&self.#indices as _),*].into_iter())
            }
        }
        Fields::Named(fields) => {
            let names = fields.named.iter().filter_map(|named| named.ident.as_ref());
            quote! {
                Box::from(vec![#(&self.#names as _),*].into_iter())
            }
        }
        Fields::Unit => quote! {
            Box::from(std::iter::empty())
        },
    }
}

fn struct_data_body(data: &DataStruct) -> TokenStream {
    match &data.fields {
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
            quote! { self.0.data() }
        }
        _ => quote! { None },
    }
}

fn enum_member_body(data: &DataEnum) -> TokenStream {
    let variants = data.variants.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            Fields::Unnamed(fields) => {
                let bindings = (0..fields.unnamed.len())
                    .map(|i| Ident::new(&format!("f{}", i), fields.span()));
                let bindings2 = bindings.clone();
                let bindings3 = bindings.clone();
                let indices = (0..fields.unnamed.len() as i64).map(|i| Literal::i64_unsuffixed(i));
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
            Fields::Named(fields) => {
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
            Fields::Unit => {
                quote! {
                    Self::#name => None
                }
            }
        }
    });
    quote! {
        match self {
            #(#variants,)*
        }
    }
}

fn enum_all_body(data: &DataEnum) -> TokenStream {
    let variants = data.variants.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            Fields::Unnamed(fields) => {
                let bindings = (0..fields.unnamed.len())
                    .map(|i| Ident::new(&format!("f{}", i), fields.span()));
                let bindings2 = bindings.clone();
                quote! {
                    Self::#name(#(#bindings),*) => Box::from(vec![#(#bindings2 as _,)*].into_iter())
                }
            }
            Fields::Named(fields) => {
                let bindings = fields.named.iter().map(|field| &field.ident);
                let bindings2 = bindings.clone();
                quote! {
                    Self::#name { #(#bindings),* } => Box::from(vec![#(#bindings2 as _,)*].into_iter())
                }
            }
            Fields::Unit => {
                quote! {
                    Self::#name => Box::from(std::iter::empty())
                }
            }
        }
    });
    quote! {
        match self {
            #(#variants,)*
        }
    }
}

fn enum_data_body(data: &DataEnum) -> TokenStream {
    let variants = data.variants.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            Fields::Unnamed(_) => {
                quote! {
                    Self::#name(..) => Some(stringify!(#name).to_string().into())
                }
            }
            Fields::Named(_) => {
                quote! {
                    Self::#name { .. } => Some(stringify!(#name).to_string().into())
                }
            }
            Fields::Unit => {
                quote! {
                    Self::#name => Some(stringify!(#name).to_string().into())
                }
            }
        }
    });
    quote! {
        match self {
            #(
                #variants,
            )*
        }
    }
}
