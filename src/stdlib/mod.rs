mod math;

use crate::interpreter::Value;
use crate::typechecker::Type;
use std::collections::HashMap;

use math as std_math;

pub type ExternalFunctionPointer = fn(&[Value]) -> Option<Value>;
pub struct ExternalGlobals {
    pub constants: HashMap<String, (Type, Value)>,
    pub functions: HashMap<String, (Type, ExternalFunctionPointer)>,
}

pub fn stdlib() -> ExternalGlobals {
    let mut constants = HashMap::default();
    constants.extend(std_math::math_constants());

    let mut functions = HashMap::default();
    functions.extend(std_math::math_functions());

    ExternalGlobals {
        constants,
        functions,
    }
}
