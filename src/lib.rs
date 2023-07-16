use lazy_static::lazy_static;
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use std::collections::HashSet;
use std::sync::Mutex;
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

fn fields_with_tag(data_struct: &DataStruct, attr_name: &str) -> (Vec<Ident>, Vec<TypePath>) {
    let mut fields = vec![];
    let mut tys = vec![];
    match &data_struct.fields {
        Fields::Named(fields_named) => {
            for field in fields_named.named.iter() {
                for attr in &field.attrs {
                    let name = attr.path();
                    if name.to_token_stream().to_string() == attr_name {
                        match &field.ty {
                            Type::Path(type_name) => {
                                let field_name = field.ident.as_ref().unwrap();
                                fields.push(field_name.clone());
                                tys.push(type_name.clone());
                            }
                            _ => panic!(),
                        };
                    }
                }
            }
        }
        _ => (),
    }
    return (fields, tys);
}

lazy_static! {
    static ref USED_COMPONENT_HASHES: Mutex<HashSet<u32>> = Mutex::new(HashSet::new());
    static ref USED_STATE_HASHES: Mutex<HashSet<u32>> = Mutex::new(HashSet::new());
}

#[proc_macro_derive(Component, attributes(base, name, buffer))]
/// All components need to derive from a BaseComponent. This macro is used to make this more
/// easily
///
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
    let field_names = field_names(data_struct);
    let (base_field_name, _) = position_field(data_struct, "base")
        .expect("The helper attribute #[component] has not been found!");

    let mut hashes = USED_COMPONENT_HASHES.lock().unwrap();
    let hash = const_fnv1a_hash::fnv1a_hash_str_32(&struct_identifier_str);
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    if hashes.contains(&hash) {
        panic!("A component with the identifier '{struct_identifier_str}' already exists!");
    }
    hashes.insert(hash);

    let (buffer_fields, buffer_types) = fields_with_tag(data_struct, "buffer");
    let mut struct_fields = Vec::with_capacity(buffer_fields.len());
    // let mut struct_values = Vec::with_capacity(buffer_fields.len());
    for (field, ty) in buffer_fields.iter().zip(buffer_types.iter()) {
        struct_fields.push(quote!(#field: #ty));
        // struct_values.push(quote!(#field: casted.#field));
    }

    let (world_def, world) = if cfg!(feature = "physics") {
        (quote!(world: &shura::physics::World,), quote!(world))
    } else {
        (quote!(), quote!())
    };

    let buffer_impl = if buffer_fields.is_empty() {
        quote!()
    } else {
        quote!(
            const INSTANCE_SIZE: u64 = (std::mem::size_of::<shura::InstanceData>() + #(std::mem::size_of::<#buffer_types>())+*) as u64;
            fn buffer(
                buffer: &mut shura::InstanceBuffer,
                #world_def
                gpu: &shura::Gpu,
                mut helper: shura::BufferHelper
            ) {

                #[repr(C)]
                #[derive(Clone, Copy)]
                struct Instance {
                    #base_field_name: shura::InstanceData,
                    #(#struct_fields),*
                }

                // Derive macro not working, so we have to manually implement bytemuck
                unsafe impl shura::bytemuck::Pod for Instance {}
                unsafe impl shura::bytemuck::Zeroable for Instance {}

                helper.buffer::<Self, Instance>(buffer, gpu, |component| {
                    Instance {
                        #base_field_name: component.#base_field_name.instance(#world),
                        #(#buffer_fields: component.#buffer_fields),*
                    }
                });
            }
        )
    };

    quote!(
        impl #impl_generics shura::FieldNames for #struct_name #ty_generics #where_clause {
            const FIELDS: &'static [&'static str] = &[#(#field_names),*];
        }

        impl #impl_generics shura::ComponentIdentifier for #struct_name #ty_generics #where_clause {
            const TYPE_NAME: &'static str = #struct_identifier;
            const IDENTIFIER: shura::ComponentTypeId = shura::ComponentTypeId::new(#hash);
        }

        impl #impl_generics shura::ComponentBuffer for #struct_name #ty_generics #where_clause {
            #buffer_impl
        }

        impl #impl_generics shura::ComponentDerive for #struct_name #ty_generics #where_clause {
            fn base(&self) -> &dyn shura::BaseComponent {
                &self.#base_field_name
            }

            fn base_mut(&mut self) -> &mut dyn shura::BaseComponent {
                &mut self.#base_field_name
            }

            fn component_type_id(&self) -> shura::ComponentTypeId {
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
    let mut hashes = USED_STATE_HASHES.lock().unwrap();
    let hash = const_fnv1a_hash::fnv1a_hash_str_32(&struct_identifier_str);
    if hashes.contains(&hash) {
        panic!("A state with the identifier '{struct_identifier_str}' already exists!");
    }
    hashes.insert(hash);

    let struct_name = ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let field_names = field_names(data_struct);

    quote!(
        impl #impl_generics StateIdentifier for #struct_name #ty_generics #where_clause {
            const TYPE_NAME: &'static str = #struct_identifier;
            const IDENTIFIER: shura::StateTypeId = shura::StateTypeId::new(#hash);
        }

        impl #impl_generics shura::FieldNames for #struct_name #ty_generics #where_clause {
            const FIELDS: &'static [&'static str] = &[#(#field_names),*];
        }

        impl #impl_generics shura::StateDerive for #struct_name #ty_generics #where_clause {

        }
    )
    .into()
}

#[proc_macro_attribute]
/// This macro helps setup a cross plattform main method
pub fn main(_args: TokenStream, item: TokenStream) -> TokenStream {
    let item: TokenStream2 = item.into();
    quote!(
        #item

        #[cfg(target_os = "android")]
        #[no_mangle]
        fn android_main(app: shura::AndroidApp) {
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
