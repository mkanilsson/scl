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
    let mut nud_names = vec![];

    for mut variant in e.variants {
        let ident = variant.ident.clone();
        let mut is_nud = false;
        let mut is_led = false;
        let mut is_stmt = false;

        for attr in &variant.attrs {
            if attr.path().is_ident("nud") {
                is_nud = true;
            } else if attr.path().is_ident("led") {
                is_led = true;
            } else if attr.path().is_ident("stmt") {
                is_stmt = true;
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

        let led_arm;

        if is_led {
            led_arm = quote::quote! { Self::#pattern => true };
        } else {
            led_arm = quote::quote! { Self::#pattern => false };
        }

        let bp_arm = if let Some(bp) = led_bp {
            quote::quote! { Self::#pattern => Some(#bp) }
        } else {
            quote::quote! { Self::#pattern => None }
        };

        let led_handler_arm = if let Some(handler) = led_handler {
            quote::quote! { Self::#pattern => Some(#handler) }
        } else {
            quote::quote! { Self::#pattern => None }
        };

        let nud_handler_arm = if let Some(handler) = nud_handler {
            quote::quote! { Self::#pattern => Some(#handler) }
        } else {
            quote::quote! { Self::#pattern => None }
        };

        let stmt_handler_arm = if let Some(handler) = stmt_handler {
            quote::quote! { Self::#pattern => Some(#handler) }
        } else {
            quote::quote! { Self::#pattern => None }
        };

        nud_arms.push(nud_arm);
        led_arms.push(led_arm);
        bp_arms.push(bp_arm);
        led_handler_arms.push(led_handler_arm);
        nud_handler_arms.push(nud_handler_arm);
        stmt_handler_arms.push(stmt_handler_arm);
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

            fn led_handler(&self) -> Option<fn(&mut Parser, lhs: Expr, bp: BindingPower) -> Expr> {
                match self {
                    #(#led_handler_arms,)*
                }
            }

            fn nud_handler(&self) -> Option<fn(&mut Parser) -> Expr> {
                match self {
                    #(#nud_handler_arms,)*
                }
            }

            fn stmt_handler(&self) -> Option<fn(&mut Parser) -> Stmt> {
                match self {
                    #(#stmt_handler_arms,)*
                }
            }

            fn nud_names() -> Vec<&'static str> {
                vec![#(#nud_names,)*]
            }
        }
    })
}

#[proc_macro_derive(Pratt, attributes(nud, led, stmt))]
pub fn pratt_derive_macro(item: TokenStream) -> TokenStream {
    metadata_derive_macro2(item.into()).unwrap().into()
}
