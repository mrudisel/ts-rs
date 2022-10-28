use std::borrow::Cow;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Field, FieldsNamed, GenericArgument, Generics, PathArguments, Result, Type};

use crate::{
    attr::{FieldAttr, Inflection, StructAttr},
    deps::Dependencies,
    types::generics::{format_generics, format_type},
    utils::{raw_name_to_ts_field, to_ts_ident},
    DerivedTS, RemapStaticVisitor,
};

pub(crate) fn named(
    attr: &StructAttr,
    name: &str,
    fields: &FieldsNamed,
    generics: &Generics,
) -> Result<DerivedTS> {
    let mut formatted_fields = vec![];
    let mut dependencies = Dependencies::default();
    if let Some(tag) = &attr.tag {
        let formatted = format!("{}: \"{}\",", tag, name);
        formatted_fields.push(quote! {
            #formatted.to_string()
        });
    }

    let mut flattened_field = None;
    for field in &fields.named {
        format_field(
            &mut formatted_fields,
            &mut dependencies,
            &mut flattened_field,
            field,
            &attr.rename_all,
            generics,
        )?;
    }

    let fields = if formatted_fields.is_empty() {
        quote!("".to_owned())
    } else {
        quote!(vec![#(#formatted_fields),*].join(" "))
    };
    let generic_args = format_generics(&mut dependencies, generics);

    let decl = if let Some(flattened) = flattened_field {
        quote!(
            format!("interface {}{} extends {} {}", #name, #generic_args, #flattened, Self::inline())
        )
    } else {
        quote!(format!("interface {}{} {}", #name, #generic_args, Self::inline()))
    };

    Ok(DerivedTS {
        inline: quote! {
            format!(
                "{{ {} }}",
                #fields,
            )
        },
        decl,
        inline_flattened: Some(fields),
        name: name.to_owned(),
        dependencies,
        export: attr.export,
        export_to: attr.export_to.clone(),
    })
}

// build an expresion which expands to a string, representing a single field of a struct.
fn format_field(
    formatted_fields: &mut Vec<TokenStream>,
    dependencies: &mut Dependencies,
    flattened_field: &mut Option<TokenStream>,
    field: &Field,
    rename_all: &Option<Inflection>,
    generics: &Generics,
) -> Result<()> {
    let FieldAttr {
        type_override,
        rename,
        inline,
        skip,
        optional,
        flatten,
        extends,
    } = FieldAttr::from_attrs(&field.attrs)?;

    if skip {
        return Ok(());
    }

    let (ty, optional_annotation) = match optional {
        true => (extract_option_argument(&field.ty)?, "?"),
        false => (&field.ty, ""),
    };

    let ty = RemapStaticVisitor::make_type_static(Cow::Borrowed(ty));

    if flatten {
        match (&type_override, &rename, inline) {
            (Some(_), _, _) => syn_err!("`type` is not compatible with `flatten`"),
            (_, Some(_), _) => syn_err!("`rename` is not compatible with `flatten`"),
            (_, _, true) => syn_err!("`inline` is not compatible with `flatten`"),
            _ => {}
        }

        if extends {
            if flattened_field.is_some() {
                syn_err!("multiple `flatten` + `extend` fields not supported");
            }

            *flattened_field = Some(quote!(<#ty as ts_rs::TS>::inline()));
        } else {
            formatted_fields.push(quote!(<#ty as ts_rs::TS>::inline_flattened()));
        }

        dependencies.append_from(&*ty);
        return Ok(());
    } else if extends {
        syn_err!("`extends` must be used with `flatten`");
    }

    let formatted_ty = type_override.map(|t| quote!(#t)).unwrap_or_else(|| {
        if inline {
            dependencies.append_from(&*ty);
            quote!(<#ty as ts_rs::TS>::inline())
        } else {
            format_type(&*ty, dependencies, generics)
        }
    });
    let field_name = to_ts_ident(field.ident.as_ref().unwrap());
    let name = match (rename, rename_all) {
        (Some(rn), _) => rn,
        (None, Some(rn)) => rn.apply(&field_name),
        (None, None) => field_name,
    };
    let valid_name = raw_name_to_ts_field(name);

    formatted_fields.push(quote! {
        format!("{}{}: {},", #valid_name, #optional_annotation, #formatted_ty)
    });

    Ok(())
}

fn extract_option_argument(ty: &Type) -> Result<&Type> {
    match ty {
        Type::Path(type_path)
            if type_path.qself.is_none()
                && type_path.path.leading_colon.is_none()
                && type_path.path.segments.len() == 1
                && type_path.path.segments[0].ident == "Option" =>
        {
            let segment = &type_path.path.segments[0];
            match &segment.arguments {
                PathArguments::AngleBracketed(args) if args.args.len() == 1 => {
                    match &args.args[0] {
                        GenericArgument::Type(inner_ty) => Ok(inner_ty),
                        _ => syn_err!("`Option` argument must be a type"),
                    }
                }
                _ => syn_err!("`Option` type must have a single generic argument"),
            }
        }
        _ => syn_err!("`optional` can only be used on an Option<T> type"),
    }
}
