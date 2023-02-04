use proc_macro::TokenStream;
use lazy_static::lazy_static;
use std::sync::Mutex;
use std::collections::HashSet;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, Type, __private::ToTokens};

fn position_field(data_struct: &DataStruct, attr_name: &str) -> Option<(String, String)> {
    match data_struct.fields {
        Fields::Named(ref fields_named) => {
            for field in fields_named.named.iter() {
                for attr in &field.attrs {
                    let name = attr.path.to_token_stream().to_string();
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


// fn name_field(ast: &DeriveInput, attr_name: &str) -> Option<String> {
//     for attr in &ast.attrs {
//         let name = attr.path.to_token_stream().to_string();
//         let value = attr.parse_args::<String>().unwrap();
//         println!("{}", value);
//     }
//     return None;
// }

lazy_static! {
    static ref USED_NAMES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
}


#[proc_macro_derive(Component, attributes(component))]
/// All ComponentControllers need to  have a component as a field.
/// This can be done using achieved using this macro.
///
/// # Example
/// ```
/// #[derive(Component)]
/// struct Bunny {
///     #[component] component: PositionComponent,
///     linvel: Vector<f32>,
/// }
/// ```
pub fn derive_component(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_name = ast.ident.to_string();

    let mut map = USED_NAMES.lock().unwrap();
    if map.contains(&struct_name) {
        panic!("A component with the struct name '{struct_name}' already exists!");
    }
    map.insert(struct_name.clone());

    let data_struct = match ast.data {
        Data::Struct(ref data_struct) => data_struct,
        _ => panic!("Must be a struct!"),
    };

    let (field_name, type_name) = position_field(data_struct, "component")
        .expect("The helper attribute #[component] has not been found!");

    // let name_field  = name_field(&ast, "component")
    //     .expect("The helper attribute #[name] has not been found!");

    format!(
        "
impl ComponentIdentifier for {struct_name} {{
    const TYPE_NAME: &'static str = \"{struct_name}\";
}}

impl ComponentDerive for {struct_name} {{
    fn base(&self) -> &dyn shura::BaseComponent {{
        &self.{field_name}
    }}

    fn base_mut(&mut self) -> &mut dyn shura::BaseComponent {{
        &mut self.{field_name}
    }}
}}

impl std::ops::Deref for {struct_name} {{
    type Target = {type_name};
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
