use lazy_static::lazy_static;
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use std::sync::Mutex;
use std::{collections::HashSet, str::FromStr};
use syn::{parse_macro_input, parse_quote, Data, DataStruct, DeriveInput, Fields, Type, TypePath};

fn field_names(data_struct: &DataStruct) -> Vec<String> {
    let mut result = vec![];
    match data_struct.fields {
        Fields::Named(ref fields_named) => {
            for field in fields_named.named.iter() {
                let field_name = field
                    .ident
                    .as_ref()
                    .and_then(|f| Some(format!("\"{0}\"", f.to_string())))
                    .unwrap();
                result.push(field_name);
            }
        }
        _ => (),
    }
    return result;
}

fn position_field(data_struct: &DataStruct, attr_name: &str) -> Option<(Ident, TypePath)> {
    match &data_struct.fields {
        Fields::Named(fields_named) => {
            for field in fields_named.named.iter() {
                for attr in &field.attrs {
                    let name = attr.path();
                    if name.to_token_stream().to_string() == attr_name {
                        match &field.ty {
                            Type::Path(type_name) => {
                                let field_name = field.ident.as_ref().unwrap();
                                return Some((field_name.clone(), type_name.clone()));
                            }
                            _ => panic!("Cannot extract the type of the component."),
                        };
                    }
                }
            }
        }
        _ => (),
    }
    None
}

fn name_field(ast: &DeriveInput, attr_name: &str) -> Option<syn::Expr> {
    for attr in &ast.attrs {
        let name = match &attr.meta {
            syn::Meta::NameValue(p) => p,
            _ => continue,
        };

        let name_str = attr.path().to_token_stream().to_string();
        if name_str == attr_name {
            return Some(name.value.clone());
        }
    }
    return None;
}

lazy_static! {
    static ref USED_COMPONENT_HASHES: Mutex<HashSet<u32>> = Mutex::new(HashSet::new());
    static ref USED_STATE_HASHES: Mutex<HashSet<u32>> = Mutex::new(HashSet::new());
}

#[proc_macro_derive(Component, attributes(base, name, collider, rigid_body))]
/// All components need to derive from the BaseComponent like the following
/// 
/// # Example:
///
/// ```
/// #[derive(Component)]
/// struct Bunny {
///     #[base] base: BaseComponent,
///     linvel: Vector<f32>,    
/// }
/// ```
pub fn derive_component(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let data_struct = match ast.data {
        Data::Struct(ref data_struct) => data_struct,
        _ => panic!("Must be a struct!"),
    };

    let struct_name = ast.ident.clone();
    let struct_name_str = struct_name.to_string();
    let struct_identifier = name_field(&ast, "name").unwrap_or(parse_quote!(#struct_name_str));
    let struct_identifier_str = struct_identifier.to_token_stream().to_string();
    let fields =
        TokenStream2::from_str(&format!("&[{}]", field_names(data_struct).join(", "))).unwrap();

    let (field_name, _) = position_field(data_struct, "base")
        .expect("The helper attribute #[component] has not been found!");

    let mut hashes = USED_COMPONENT_HASHES.lock().unwrap();
    let hash = const_fnv1a_hash::fnv1a_hash_str_32(&struct_identifier_str);
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    if hashes.contains(&hash) {
        panic!("A component with the identifier '{struct_identifier_str}' already exists!");
    }
    hashes.insert(hash);

    quote!(
        impl #impl_generics shura::FieldNames for #struct_name #ty_generics #where_clause {
            const FIELDS: &'static [&'static str] = #fields;
        }

        impl #impl_generics shura::ComponentIdentifier for #struct_name #ty_generics #where_clause {
            const TYPE_NAME: &'static str = #struct_identifier;
            const IDENTIFIER: shura::ComponentTypeId = shura::ComponentTypeId::new(#hash);
        }

        impl #impl_generics shura::ComponentDerive for #struct_name #ty_generics #where_clause {
            fn base(&self) -> &dyn BaseComponent {
                &self.#field_name
            }

            fn component_type_id(&self) -> ComponentTypeId {
                shura::ComponentTypeId::new(#hash)
            }
        }
    )
    .into()
}

#[proc_macro_derive(State, attributes(name, priority))]
/// All scene- and globalstates must derive from this macro.
/// 
/// # Example:
/// 
/// ```
/// #[derive(State)]
/// struct MySceneStates {
///     shared_model: Model
/// }
/// ```
pub fn derive_state(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let data_struct = &match ast.data {
        Data::Struct(ref data_struct) => data_struct,
        _ => panic!("Must be a struct!"),
    };

    let struct_name = ast.ident.clone();
    let struct_name_str = struct_name.to_string();
    let struct_identifier = name_field(&ast, "name").unwrap_or(parse_quote!(#struct_name_str));
    let struct_identifier_str = struct_identifier.to_token_stream().to_string();
    let priority = name_field(&ast, "priority").unwrap_or(parse_quote!(0));
    let mut hashes = USED_STATE_HASHES.lock().unwrap();
    let hash = const_fnv1a_hash::fnv1a_hash_str_32(&struct_identifier_str);
    if hashes.contains(&hash) {
        panic!("A state with the identifier '{struct_identifier_str}' already exists!");
    }
    hashes.insert(hash);

    let struct_name = ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let fields =
        TokenStream2::from_str(&format!("&[{}]", field_names(data_struct).join(", "))).unwrap();

    quote!(
        impl #impl_generics StateIdentifier for #struct_name #ty_generics #where_clause {
            const TYPE_NAME: &'static str = #struct_identifier;
            const IDENTIFIER: shura::StateTypeId = shura::StateTypeId::new(#hash);
            const PRIORITY: i16 = #priority;
        }

        impl #impl_generics shura::FieldNames for #struct_name #ty_generics #where_clause {
            const FIELDS: &'static [&'static str] = #fields;
        }
    )
    .into()
}

#[proc_macro_attribute]
pub fn main(_args: TokenStream, item: TokenStream) -> TokenStream {
    let item: TokenStream2 = item.into();
    quote!(
        #item

        #[cfg(target_os = "android")]
        #[no_mangle]
        fn android_main(app: AndroidApp) {
            shura_main(shura::ShuraConfig::default(app));
        }

        #[cfg(not(target_os = "android"))]
        #[allow(dead_code)]
        fn main() {
            shura_main(shura::ShuraConfig::default());
        }
    )
    .into()
}
