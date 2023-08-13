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

fn position_field(data_struct: &DataStruct, attr_name: &str) -> Option<Ident> {
    match &data_struct.fields {
        Fields::Named(fields_named) => {
            for field in fields_named.named.iter() {
                for attr in &field.attrs {
                    let name = attr.path();
                    if name.to_token_stream().to_string() == attr_name {
                        match &field.ty {
                            Type::Path(_type_name) => {
                                let field_name = field.ident.as_ref().unwrap();
                                return Some(field_name.clone());
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

#[proc_macro_derive(Component, attributes(position, name, buffer))]
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
    let (has_position, position_field_name) = position_field(data_struct, "position")
        .map(|f| (true, quote!(#f)))
        .unwrap_or((false, quote!(EMPTY_DEFAULT_COMPONENT)));
    let var_position_field_name = if has_position {
        quote!(self.#position_field_name)
    } else {
        quote!(shura::#position_field_name)
    };

    let mut hashes = USED_COMPONENT_HASHES.lock().unwrap();
    let mut hash = const_fnv1a_hash::fnv1a_hash_str_32(&struct_identifier_str);
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    while hashes.contains(&hash) {
        hash += 1;
    }
    hashes.insert(hash);

    let (buffer_fields, buffer_types) = fields_with_tag(data_struct, "buffer");
    let mut struct_fields = Vec::with_capacity(buffer_fields.len());
    for (field, ty) in buffer_fields.iter().zip(buffer_types.iter()) {
        struct_fields.push(quote!(#field: #ty));
    }

    let buffer_impl = if buffer_fields.is_empty() {
        quote!()
    } else {
        quote!(
            const INSTANCE_SIZE: u64 = (std::mem::size_of::<shura::InstancePosition>() + #(std::mem::size_of::<#buffer_types>())+*) as u64;
            fn buffer_with(
                gpu: &shura::Gpu,
                mut helper: shura::BufferHelper,
                each: impl Fn(&mut Self) + Send + Sync
            ) {
                use shura::bytemuck;
                #[repr(C)]
                #[derive(Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
                struct Instance {
                    #position_field_name: shura::InstancePosition,
                    #(#struct_fields),*
                }

                helper.buffer::<Self, Instance>(gpu, |component| {
                    (each)(component);
                    Instance {
                        #position_field_name: component.position().instance(helper.world),
                        #(#buffer_fields: component.#buffer_fields),*
                    }
                });
            }

            fn buffer(
                gpu: &shura::Gpu,
                mut helper: shura::BufferHelper
            ) {
                use shura::bytemuck;
                #[repr(C)]
                #[derive(Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
                struct Instance {
                    #position_field_name: shura::InstancePosition,
                    #(#struct_fields),*
                }

                helper.buffer::<Self, Instance>(gpu, |component| {
                    Instance {
                        #position_field_name: component.position().instance(helper.world),
                        #(#buffer_fields: component.#buffer_fields),*
                    }
                });
            }
        )
    };

    let init_finish = if has_position {
        quote!(
            fn init(&mut self, handle: shura::ComponentHandle, world: &mut shura::World) {
                #var_position_field_name.init(handle, world)
            }

            fn finish(&mut self, world: &mut shura::World) {
                #var_position_field_name.finish(world)
            }
        )
    } else {
        quote!(
            fn init(&mut self, handle: shura::ComponentHandle, world: &mut shura::World) {}
            fn finish(&mut self, world: &mut shura::World) {}
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
            fn position(&self) -> &dyn shura::Position {
                &#var_position_field_name
            }

            #init_finish

            fn component_type_id(&self) -> shura::ComponentTypeId {
                Self::IDENTIFIER
            }
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
