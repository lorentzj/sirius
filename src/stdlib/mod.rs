mod math;

use crate::interpreter::Value;
use crate::typechecker::Type;
use std::collections::HashMap;

use math as std_math;

pub type ExternalGlobals = HashMap<String, (Type, Value)>;

pub fn stdlib() -> ExternalGlobals {
    let mut vals = HashMap::default();
    vals.extend(std_math::math_mod());

    vals
}
