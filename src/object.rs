#[derive(Debug, Clone, PartialEq)]
struct Obj {} // Anything heap-allocated is an obj

struct ObjString {
    obj: Obj,
    string: String,
}
