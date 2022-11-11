#![feature(let_chains)]
#![macro_use]
#![deny(unused)]

use std::borrow::Cow;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse_quote, spanned::Spanned, ConstParam, GenericArgument, GenericParam, Generics, Item,
    LifetimeDef, Result, Type, TypeParam, WhereClause, TypeReference, PathArguments,
};

use crate::deps::Dependencies;

#[macro_use]
mod utils;
mod attr;
mod deps;
mod types;

struct DerivedTS {
    name: String,
    inline: TokenStream,
    decl: TokenStream,
    inline_flattened: Option<TokenStream>,
    dependencies: Dependencies,

    export: bool,
    export_to: Option<String>,
}

impl DerivedTS {
    fn generate_export_test(&self, rust_ty: &Ident, generics: &Generics) -> Option<TokenStream> {
        let test_fn = format_ident!("export_bindings_{}", &self.name.to_lowercase());
        let generic_params = generics
            .params
            .iter()
            .filter(|param| matches!(param, GenericParam::Type(_)))
            .map(|_| quote! { () });
        let ty = quote!(<#rust_ty<#(#generic_params),*> as ts_rs::TS>);

        Some(quote! {
            #[cfg(test)]
            #[test]
            fn #test_fn() {
                #ty::export().expect("could not export type");
            }
        })
    }

    fn into_impl(self, rust_ty: Ident, generics: Generics) -> TokenStream {
        let mut base_path =
            std::env::var("TS_RS_BASE_EXPORT_PATH").unwrap_or_else(|_| String::new());

        if !base_path.is_empty() && !base_path.ends_with('/') {
            base_path.push_str("/");
        }

        let export_to = match &self.export_to {
            Some(dirname) if dirname.ends_with('/') => {
                format!("{base_path}{}{}.ts", dirname, self.name)
            }
            Some(filename) => format!("{base_path}{filename}"),
            None => {
                format!("{base_path}bindings/{}.ts", self.name)
            }
        };

        let export = match self.export {
            true => Some(self.generate_export_test(&rust_ty, &generics)),
            false => None,
        };

        let DerivedTS {
            name,
            inline,
            decl,
            inline_flattened,
            dependencies,
            ..
        } = self;
        let inline_flattened = inline_flattened
            .map(|t| {
                quote! {
                    fn inline_flattened() -> String {
                        #t
                    }
                }
            });

        let impl_start = generate_impl(&rust_ty, &generics);
        quote! {
            #impl_start {
                const EXPORT_TO: Option<&'static str> = Some(#export_to);

                fn decl() -> String {
                    #decl
                }
                fn name() -> String {
                    #name.to_owned()
                }
                fn inline() -> String {
                    #inline
                }
                #inline_flattened
                fn dependencies() -> Vec<ts_rs::Dependency> {
                    #dependencies
                }
                fn transparent() -> bool {
                    false
                }
            }

            #export
        }
    }
}

// generate start of the `impl TS for #ty` block, up to (excluding) the open brace
fn generate_impl(ty: &Ident, generics: &Generics) -> TokenStream {
    use GenericParam::*;

    let bounds = generics.params.iter().filter_map(|param| match param {
        Type(TypeParam {
            ident,
            colon_token,
            bounds,
            ..
        }) => Some(quote!(#ident #colon_token #bounds)),
        Lifetime(LifetimeDef { .. }) => None,
        Const(ConstParam {
            const_token,
            ident,
            colon_token,
            ty,
            ..
        }) => Some(quote!(#const_token #ident #colon_token #ty)),
    });
    let type_args = generics.params.iter().map(|param| match param {
        Type(TypeParam { ident, .. }) | Const(ConstParam { ident, .. }) => quote!(#ident),
        Lifetime(LifetimeDef { .. }) => quote!('static),
    });

    let where_bound = add_ts_to_where_clause(generics);
    quote!(impl <#(#bounds),*> ts_rs::TS for #ty <#(#type_args),*> #where_bound)
}

fn add_ts_to_where_clause(generics: &Generics) -> Option<WhereClause> {
    let generic_types = generics
        .params
        .iter()
        .filter_map(|gp| match gp {
            GenericParam::Type(ty) => Some(ty.ident.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    if generic_types.is_empty() {
        return generics.where_clause.clone();
    }
    match generics.where_clause {
        None => Some(parse_quote! { where #( #generic_types : ts_rs::TS ),* }),
        Some(ref w) => {
            let bounds = w.predicates.iter();
            Some(parse_quote! { where #(#bounds,)* #( #generic_types : ts_rs::TS ),* })
        }
    }
}

/// Derives [TS](./trait.TS.html) for a struct or enum.
/// Please take a look at [TS](./trait.TS.html) for documentation.
#[proc_macro_derive(TS, attributes(ts))]
pub fn typescript(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match entry(input) {
        Err(err) => err.to_compile_error(),
        Ok(result) => result,
    }
    .into()
}

fn entry(input: proc_macro::TokenStream) -> Result<TokenStream> {
    let input = syn::parse::<Item>(input)?;
    let (ts, ident, generics) = match input {
        Item::Struct(s) => (types::struct_def(&s)?, s.ident, s.generics),
        Item::Enum(e) => (types::enum_def(&e)?, e.ident, e.generics),
        _ => syn_err!(input.span(); "unsupported item"),
    };

    Ok(ts.into_impl(ident, generics))
}

// Remap all lifetimes to 'static in ty.
struct RemapStaticVisitor;

impl RemapStaticVisitor {
    fn make_type_static<'a, T>(ty: T) -> Cow<'a, Type>
    where
        T: Into<Cow<'a, Type>>,
    {
        let mut ty = ty.into();
        if let Type::Reference(refer) = &*ty 
            && let Some(lt) = refer.lifetime.as_ref()
            && lt.ident != "static"
        {
            syn::visit_mut::VisitMut::visit_type_mut(&mut Self, ty.to_mut());
        } else if matches!(*ty, Type::Path(_)) {
            syn::visit_mut::VisitMut::visit_type_mut(&mut Self, ty.to_mut());            
        }

        ty
    }

    #[allow(dead_code)]
    fn make_generic_static<'a, G>(gen: G) -> Cow<'a, GenericArgument>
    where
        G: Into<Cow<'a, GenericArgument>>,
    {
        let mut gen: Cow<'a, GenericArgument> = gen.into();

        if let GenericArgument::Lifetime(lt) = &*gen 
            && lt.ident != "static"
        {
            syn::visit_mut::VisitMut::visit_generic_argument_mut(&mut Self, gen.to_mut());
        }

        gen
    }
}

impl syn::visit_mut::VisitMut for RemapStaticVisitor {
    fn visit_type_mut(&mut self, ty: &mut Type) {
        match ty {
            Type::Reference(TypeReference { lifetime: Some(lt), .. }) if lt.ident != "static" => {
                *lt = syn::parse2(quote!('static)).unwrap();
            }
            Type::Path(path) => {
                for segment in path.path.segments.iter_mut() { 
                    if let PathArguments::AngleBracketed(args) = &mut segment.arguments {
                        for arg in args.args.iter_mut() {
                            match arg { 
                                GenericArgument::Lifetime(lt) => {
                                    *lt = syn::parse2(quote!('static)).unwrap();
                                },
                                // recurse any nested types (happens in cases like 'Vec<Cow<'_, str>>')
                                GenericArgument::Type(ty) => self.visit_type_mut(ty),
                                _ => {}
                            }   
                        }
                    }
                }
            } 
            _ => {}
        } 
    }

    fn visit_generic_argument_mut(&mut self, ga: &mut GenericArgument) {
        match ga {
            GenericArgument::Lifetime(lt) => {
                *lt = syn::parse2(quote!('static)).unwrap();
            }
            _ => {}
        }
    }
}
