# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# HACK: LLDB's python API doesn't afford anything helpful for getting at variadic template parameters.
# We're forced to resort to parsing names as strings.
def templateParams(s):
    depth = 0
    start = s.find('<') + 1
    result = []
    for i, c in enumerate(s[start:], start):
        if c == '<':
            depth += 1
        elif c == '>':
            if depth == 0:
                result.append(s[start: i].strip())
                break
            depth -= 1
        elif c == ',' and depth == 0:
            result.append(s[start: i].strip())
            start = i + 1
    return result

def getType(target, typeName):
    stars = 0

    typeName = typeName.strip()
    while typeName.endswith('*'):
        stars += 1
        typeName = typeName[:-1]

    if typeName.startswith('const '):
        typeName = typeName[6:]

    ty = target.FindFirstType(typeName.strip())
    for _ in range(stars):
        ty = ty.GetPointerType()

    return ty

def luau_variant_summary(valobj, internal_dict, options):
    type_id = valobj.GetChildMemberWithName("typeId").GetValueAsUnsigned()
    storage = valobj.GetChildMemberWithName("storage")
    params = templateParams(valobj.GetType().GetCanonicalType().GetName())
    stored_type = params[type_id]
    value = storage.Cast(stored_type.GetPointerType()).Dereference()
    return stored_type.GetDisplayTypeName() + " [" + value.GetValue() + "]"

class LuauVariantSyntheticChildrenProvider:
    node_names = ["type", "value"]

    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.type_index = None
        self.current_type = None
        self.type_params = []
        self.stored_value = None

    def num_children(self):
        return len(self.node_names)

    def has_children(self):
        return True

    def get_child_index(self, name):
        try:
            return self.node_names.index(name)
        except ValueError:
            return -1

    def get_child_at_index(self, index):
        try:
            node = self.node_names[index]
        except IndexError:
            return None

        if node == "type":
            if self.current_type:
                return self.valobj.CreateValueFromExpression(node, f"(const char*)\"{self.current_type.GetDisplayTypeName()}\"")
            else:
                return self.valobj.CreateValueFromExpression(node, "(const char*)\"<unknown type>\"")
        elif node == "value":
            if self.stored_value is not None:
                if self.current_type is not None:
                    return self.valobj.CreateValueFromData(node, self.stored_value.GetData(), self.current_type)
                else:
                    return self.valobj.CreateValueExpression(node, "(const char*)\"<unknown type>\"")
            else:
                return self.valobj.CreateValueFromExpression(node, "(const char*)\"<no stored value>\"")
        else:
            return None

    def update(self):
        self.type_index = self.valobj.GetChildMemberWithName("typeId").GetValueAsSigned()
        self.type_params = templateParams(self.valobj.GetType().GetCanonicalType().GetName())

        if len(self.type_params) > self.type_index:
            self.current_type = getType(self.valobj.GetTarget(), self.type_params[self.type_index])

            if self.current_type:
                storage = self.valobj.GetChildMemberWithName("storage")
                self.stored_value = storage.Cast(self.current_type.GetPointerType()).Dereference()
            else:
                self.stored_value = None
        else:
            self.current_type = None
            self.stored_value = None

        return False
