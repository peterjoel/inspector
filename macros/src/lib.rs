use proc_macro2::{Literal, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput, Fields, Ident,
};

#[proc_macro_derive(Queryable)]
pub fn derive_queryable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impl_queryable(parse_macro_input!(input as DeriveInput)).into()
}

fn impl_queryable(input: DeriveInput) -> TokenStream {
    let name = &input.ident;
    // let generics = &input.generics;
    let (keys_body, member_body, all_body, data_body) = match &input.data {
        Data::Struct(data) => (
            struct_keys_body(data),
            struct_member_body(data),
            struct_all_body(data),
            struct_data_body(data),
        ),
        Data::Enum(data) => (
            enum_keys_body(data),
            enum_member_body(data),
            enum_all_body(data),
            enum_data_body(data),
        ),
        Data::Union(_) => return quote! { compile_error!("Unions are not supported"); },
    };
    quote! {
        // TODO support generics
        impl<'a> clouseau::Queryable<'a> for #name {
            fn keys(&'a self) -> Box<dyn Iterator<Item = clouseau::Value> + 'a> {
                #keys_body
            }
            fn member<'f>(&'a self, field_name: &'f clouseau::Value) -> Option<&'a dyn clouseau::Queryable<'a>> {
                #member_body
            }
            fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn clouseau::Queryable<'a>> + 'a> {
                #all_body
            }
            fn data(&self) -> Option<clouseau::Value> {
                #data_body
            }
        }
    }
}

fn struct_keys_body(data: &DataStruct) -> TokenStream {
    let keys = data.fields.iter().enumerate().map(|(index, field)| {
        if let Some(ident) = &field.ident {
            let key = ident.to_string();
            quote! {
                clouseau::Value::String(#key.to_string())
            }
        } else {
            let index = Literal::i64_unsuffixed(index as i64);
            quote! {
                clouseau::Value::Int(#index)
            }
        }
    });
    quote! {
        Box::from(vec![#(#keys),*].into_iter())
    }
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
                            clouseau::Value::Int(#indices) => { Some(&self.#indices as _) },
                        )*
                        clouseau::Value::String(s) => match s.parse::<usize>() {
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
                    clouseau::Value::String(s) => {
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

fn enum_keys_body(data: &DataEnum) -> TokenStream {
    let variants = data.variants.iter().map(|variant| {
        let name = &variant.ident;
        match &variant.fields {
            Fields::Unnamed(fields) => {
                let indices = (0..fields.unnamed.len() as i64).map(|i| Literal::i64_unsuffixed(i));
                quote! {
                    Self::#name(..) => Box::from(vec![#(clouseau::Value::Int(#indices)),*].into_iter())
                }
            }
            Fields::Named(fields) => {
                let names = fields
                    .named
                    .iter()
                    .filter_map(|field| field.ident.as_ref())
                    .map(Ident::to_string);
                quote! {
                    Self::#name { .. } => Box::from(vec![#(clouseau::Value::from(#names)),*].into_iter())
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
            #(#variants),*
        }
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
                                clouseau::Value::Int(#indices) => { Some(#bindings2 as _) },
                            )*
                            clouseau::Value::String(s) => if let Ok(i) = s.parse::<i64>() {
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
                            clouseau::Value::String(s) => match s.as_str() {
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
                    Self::#name(..) => Some(clouseau::Value::String(stringify!(#name).to_string()))
                }
            }
            Fields::Named(_) => {
                quote! {
                    Self::#name { .. } => Some(clouseau::Value::String(stringify!(#name).to_string()))
                }
            }
            Fields::Unit => {
                quote! {
                    Self::#name => Some(clouseau::Value::String(stringify!(#name).to_string()))
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
