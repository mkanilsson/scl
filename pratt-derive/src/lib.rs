use proc_macro::TokenStream;
use syn::{Data, DeriveInput};

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(nud))]
struct NudAttribute(syn::Expr);

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(led))]
struct LedAttribute(syn::Expr, syn::Expr);

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(stmt))]
struct StmtAttribute(syn::Expr);

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(name))]
struct NameAttribute(String);

fn metadata_derive_macro2(
    item: proc_macro2::TokenStream,
) -> deluxe::Result<proc_macro2::TokenStream> {
    // Parse
    let ast: DeriveInput = syn::parse2(item)?;

    let Data::Enum(e) = ast.data else {
        return Err(syn::Error::new_spanned(ast.ident, "Pratt derive only works on enums").into());
    };

    let mut nud_arms = vec![];
    let mut led_arms = vec![];
    let mut bp_arms = vec![];
    let mut led_handler_arms = vec![];
    let mut nud_handler_arms = vec![];
    let mut stmt_handler_arms = vec![];
    let mut name_arms = vec![];
    let mut nud_names = vec![];

    for mut variant in e.variants {
        let ident = variant.ident.clone();
        let mut is_nud = false;
        let mut is_led = false;
        let mut is_stmt = false;
        let mut is_name = false;

        for attr in &variant.attrs {
            if attr.path().is_ident("nud") {
                is_nud = true;
            } else if attr.path().is_ident("led") {
                is_led = true;
            } else if attr.path().is_ident("stmt") {
                is_stmt = true;
            } else if attr.path().is_ident("name") {
                is_name = true;
            }
        }

        let mut nud_handler = None;

        if is_nud {
            let attrs: NudAttribute = deluxe::extract_attributes(&mut variant)?;
            nud_handler = Some(attrs.0);
            let ident_str = variant.ident.to_string();
            nud_names.push(quote::quote! { #ident_str });
        }

        let mut led_bp = None;
        let mut led_handler = None;

        if is_led {
            let attrs: LedAttribute = deluxe::extract_attributes(&mut variant)?;
            led_bp = Some(attrs.0);
            led_handler = Some(attrs.1);
        }

        let mut stmt_handler = None;

        if is_stmt {
            let attrs: StmtAttribute = deluxe::extract_attributes(&mut variant)?;
            stmt_handler = Some(attrs.0);
        }
        let name = if is_name {
            let attrs: NameAttribute = deluxe::extract_attributes(&mut variant)?;
            attrs.0
        } else {
            variant.ident.to_string().to_lowercase()
        };

        let pattern = match variant.fields {
            syn::Fields::Named(_) => quote::quote! { #ident{..} },
            syn::Fields::Unnamed(_) => quote::quote! { #ident(_) },
            syn::Fields::Unit => quote::quote! { #ident },
        };

        let nud_arm = if is_nud {
            quote::quote! { Self::#pattern => true }
        } else {
            quote::quote! { Self::#pattern => false }
        };

        let led_arm = if is_led {
            quote::quote! { Self::#pattern => true }
        } else {
            quote::quote! { Self::#pattern => false }
        };

        nud_arms.push(nud_arm);
        led_arms.push(led_arm);
        bp_arms.push(optional_to_arm(&variant, led_bp));
        led_handler_arms.push(optional_to_arm(&variant, led_handler));
        nud_handler_arms.push(optional_to_arm(&variant, nud_handler));
        stmt_handler_arms.push(optional_to_arm(&variant, stmt_handler));
        name_arms.push(quote::quote! { Self::#pattern => #name });
    }

    // define impl variables
    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();

    // generate
    Ok(quote::quote! {
        impl #impl_generics Pratt for #ident #type_generics #where_clause {
            fn is_nud(&self) -> bool {
                match self {
                    #(#nud_arms,)*
                }
            }

            fn is_led(&self) -> bool {
                match self {
                    #(#led_arms,)*
                }
            }

            fn binding_power(&self) -> Option<BindingPower> {
                match self {
                    #(#bp_arms,)*
                }
            }

            fn led_handler(&self) -> Option<fn(&mut Parser, lhs: Expr, bp: BindingPower) -> Result<Expr>> {
                match self {
                    #(#led_handler_arms,)*
                }
            }

            fn nud_handler(&self) -> Option<fn(&mut Parser) -> Result<Expr>> {
                match self {
                    #(#nud_handler_arms,)*
                }
            }

            fn stmt_handler(&self) -> Option<fn(&mut Parser) -> Result<Stmt>> {
                match self {
                    #(#stmt_handler_arms,)*
                }
            }

            fn nud_names() -> Vec<&'static str> {
                vec![#(#nud_names,)*]
            }

            fn name(&self) -> &'static str {
                match self {
                    #(#name_arms,)*
                }
            }
        }
    })
}

fn optional_to_arm(variant: &syn::Variant, arm: Option<syn::Expr>) -> proc_macro2::TokenStream {
    let ident = variant.ident.clone();

    let pattern = match variant.fields {
        syn::Fields::Named(_) => quote::quote! { #ident{..} },
        syn::Fields::Unnamed(_) => quote::quote! { #ident(_) },
        syn::Fields::Unit => quote::quote! { #ident },
    };

    if let Some(handler) = arm {
        quote::quote! { Self::#pattern => Some(#handler) }
    } else {
        quote::quote! { Self::#pattern => None }
    }
}

#[proc_macro_derive(Pratt, attributes(nud, led, stmt, name))]
pub fn pratt_derive_macro(item: TokenStream) -> TokenStream {
    metadata_derive_macro2(item.into()).unwrap().into()
}
