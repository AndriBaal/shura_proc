use lazy_static::lazy_static;
use proc_macro::TokenStream;
use std::collections::HashSet;
use std::sync::Mutex;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, Type, __private::ToTokens};

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

fn position_field(data_struct: &DataStruct, attr_name: &str) -> Option<(String, String)> {
    match data_struct.fields {
        Fields::Named(ref fields_named) => {
            for field in fields_named.named.iter() {
                for attr in &field.attrs {
                    let name = attr.path().to_token_stream().to_string();
                    if name == attr_name {
                        match field.ty {
                            Type::Path(ref p) => {
                                let field_name = field.ident.as_ref().unwrap();
                                let type_name = p.to_token_stream().to_string();
                                return Some((field_name.to_string(), type_name.to_string()));
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

fn name_field(ast: &DeriveInput, attr_name: &str) -> Option<String> {
    for attr in &ast.attrs {
        let name = attr.path().to_token_stream().to_string();
        if name == attr_name {
            let mut value = attr.to_token_stream().to_string();
            assert!(
                value.len() >= 3,
                "You need to provide a name like the following pattern: '#[name(Bunny)]'!"
            );
            value.pop();
            value.remove(0);
            return Some(value);
        }
    }
    return None;
}

lazy_static! {
    static ref USED_HASHES: Mutex<HashSet<u32>> = Mutex::new(HashSet::new());
}

#[proc_macro_derive(Component, attributes(component, name))]
/// All components need to derive from the BaseComponent like the following
/// example:
/// 
/// ```
/// #[derive(Component)]
/// struct Bunny {
///     #[component] component: BaseComponent,
///     linvel: Vector<f32>,
/// }
/// ```
pub fn derive_component(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let data_struct = match ast.data {
        Data::Struct(ref data_struct) => data_struct,
        _ => panic!("Must be a struct!"),
    };

    let struct_name = ast.ident.to_string();
    let struct_identifier = name_field(&ast, "name").unwrap_or(struct_name.clone());
    let fields = format!("&[{}]", field_names(data_struct).join(", "));

    let mut hashes = USED_HASHES.lock().unwrap();
    let hash = const_fnv1a_hash::fnv1a_hash_str_32(&struct_identifier);
    if hashes.contains(&hash) {
        panic!("A component with the struct identifier '{struct_identifier}' already exists!");
    }
    hashes.insert(hash);

    let (field_name, base_name) = position_field(data_struct, "component")
        .expect("The helper attribute #[component] has not been found!");

    format!(
        "
impl shura::FieldNames for {struct_name} {{
    const FIELDS: &'static [&'static str] = {fields};
}}

impl shura::ComponentIdentifier for {struct_name} {{
    const TYPE_NAME: &'static str = \"{struct_identifier}\";
    const IDENTIFIER: shura::ComponentTypeId = shura::ComponentTypeId::new({hash});
}}

impl shura::ComponentDerive for {struct_name} {{
    fn base(&self) -> &{base_name} {{
        &self.{field_name}
    }}

    fn base_mut(&mut self) -> &mut {base_name} {{
        &mut self.{field_name}
    }}
}}

impl std::ops::Deref for {struct_name} {{
    type Target = {base_name};
    fn deref(&self) -> &Self::Target {{
        &self.{field_name}
    }}
}}

impl std::ops::DerefMut for {struct_name} {{
    fn deref_mut(&mut self) -> &mut Self::Target {{
        &mut self.{field_name}
    }}
}}
    ",
    )
    .parse()
    .unwrap()
}

#[proc_macro_derive(State, attributes(component, name))]
pub fn derive_state(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let data_struct = match ast.data {
        Data::Struct(ref data_struct) => data_struct,
        _ => panic!("Must be a struct!"),
    };

    let struct_name = ast.ident.to_string();
    let fields = format!("&[{}]", field_names(data_struct).join(", "));

    format!(
        "
impl shura::FieldNames for {struct_name} {{
    const FIELDS: &'static [&'static str] = {fields};
}}
    ",
    )
    .parse()
    .unwrap()
}
