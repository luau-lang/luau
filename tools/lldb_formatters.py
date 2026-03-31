# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

import lldb

# HACK: LLDB's python API doesn't afford anything helpful for getting at variadic template parameters.
# We're forced to resort to parsing names as strings.

def read_non_cstring_from_data(data):
    str_text = ""
    for c in data.uint8s:
        str_text += chr(c)

    return str_text

def create_quoted_escaped_c_str(s):
    """Given a string, this function quotes the string and escapes any special characters (e.g. '\n', '\t')"""
    return f'"{repr(s)[1:-1]}"'

def templateParams(s):
    depth = 0
    start = s.find("<") + 1
    result = []
    for i, c in enumerate(s[start:], start):
        if c == "<":
            depth += 1
        elif c == ">":
            if depth == 0:
                result.append(s[start:i].strip())
                break
            depth -= 1
        elif c == "," and depth == 0:
            result.append(s[start:i].strip())
            start = i + 1
    return result


def getType(target, typeName):
    stars = 0

    typeName = typeName.strip()
    while typeName.endswith("*"):
        stars += 1
        typeName = typeName[:-1]

    if typeName.startswith("const "):
        typeName = typeName[6:]

    ty = target.FindFirstType(typeName.strip())
    for _ in range(stars):
        ty = ty.GetPointerType()

    return ty


def luau_variant_summary(valobj, internal_dict, options):
    return valobj.GetChildMemberWithName("type").GetSummary()[1:-1]


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
                return self.valobj.CreateValueFromExpression(
                    node, f'(const char*)"{self.current_type.GetDisplayTypeName()}"'
                )
            else:
                return self.valobj.CreateValueFromExpression(
                    node, '(const char*)"<unknown type>"'
                )
        elif node == "value":
            if self.stored_value is not None:
                if self.current_type is not None:
                    return self.valobj.CreateValueFromData(
                        node, self.stored_value.GetData(), self.current_type
                    )
                else:
                    return self.valobj.CreateValueExpression(
                        node, '(const char*)"<unknown type>"'
                    )
            else:
                return self.valobj.CreateValueFromExpression(
                    node, '(const char*)"<no stored value>"'
                )
        else:
            return None

    def update(self):
        self.type_index = self.valobj.GetChildMemberWithName(
            "typeId"
        ).GetValueAsSigned()
        self.type_params = templateParams(
            self.valobj.GetType().GetCanonicalType().GetName()
        )

        if len(self.type_params) > self.type_index:
            self.current_type = getType(
                self.valobj.GetTarget(), self.type_params[self.type_index]
            )

            if self.current_type:
                storage = self.valobj.GetChildMemberWithName("storage")
                self.stored_value = storage.Cast(self.current_type)
            else:
                self.stored_value = None
        else:
            self.current_type = None
            self.stored_value = None

        return False


class DenseHashTableSyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        """this call should initialize the Python object using valobj as the variable to provide synthetic children for"""
        self.valobj = valobj
        self.update()

    def num_children(self):
        """this call should return the number of children that you want your object to have"""
        return self.capacity

    def get_child_index(self, name):
        """this call should return the index of the synthetic child whose name is given as argument"""
        try:
            if name.startswith("[") and name.endswith("]"):
                return int(name[1:-1])
            else:
                return -1
        except Exception as e:
            print("get_child_index exception", e)
            return -1

    def get_child_at_index(self, index):
        """this call should return a new LLDB SBValue object representing the child at the index given as argument"""
        try:
            dataMember = self.valobj.GetChildMemberWithName("data")

            data = dataMember.GetPointeeData(index)

            return self.valobj.CreateValueFromData(
                f"[{index}]",
                data,
                dataMember.Dereference().GetType(),
            )

        except Exception as e:
            print("get_child_at_index error", e)

    def update(self):
        """this call should be used to update the internal state of this Python object whenever the state of the variables in LLDB changes.[1]
        Also, this method is invoked before any other method in the interface."""
        self.capacity = self.valobj.GetChildMemberWithName(
            "capacity"
        ).GetValueAsUnsigned()

    def has_children(self):
        """this call should return True if this object might have children, and False if this object can be guaranteed not to have children.[2]"""
        return True


class DenseHashMapSyntheticChildrenProvider:
    fixed_names = ["count", "capacity"]
    max_expand_children = 100
    max_expand_capacity = 1000

    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.count = 0
        self.capacity = 0

    def num_children(self):
        return min(self.max_expand_children, self.count) + len(self.fixed_names)

    def get_child_index(self, name):
        try:
            if name in self.fixed_names:
                return self.fixed_names.index(name)

            return -1
        except Exception as e:
            print("get_child_index exception", e, name)
            return -1

    def get_child_at_index(self, index):
        try:
            if index < len(self.fixed_names):
                fixed_name = self.fixed_names[index]
                impl_child = self.valobj.GetValueForExpressionPath(
                    f".impl.{fixed_name}")

                return self.valobj.CreateValueFromData(fixed_name, impl_child.GetData(), impl_child.GetType())
            else:
                index -= len(self.fixed_names)

            empty_key_valobj = self.valobj.GetValueForExpressionPath(
                f".impl.empty_key")
            key_type = empty_key_valobj.GetType().GetCanonicalType().GetName()
            skipped = 0

            for slot in range(0, min(self.max_expand_capacity, self.capacity)):
                slot_pair = self.valobj.GetValueForExpressionPath(
                    f".impl.data[{slot}]")
                slot_key_valobj = slot_pair.GetChildMemberWithName("first")

                eq_test_valobj = self.valobj.EvaluateExpression(
                    f"*(reinterpret_cast<const {key_type}*>({empty_key_valobj.AddressOf().GetValueAsUnsigned()})) == *(reinterpret_cast<const {key_type}*>({slot_key_valobj.AddressOf().GetValueAsUnsigned()}))")
                if eq_test_valobj.GetValue() == "true":
                    continue

                # Skip over previous occupied slots.
                if index > skipped:
                    skipped += 1
                    continue

                return self.valobj.CreateValueFromData(f"[{index}]", slot_pair.GetData(), slot_pair.GetType())

        except Exception as e:
            print("get_child_at_index error", e, index)

    def update(self):
        try:
            self.capacity = self.count = self.valobj.GetValueForExpressionPath(
                ".impl.capacity").GetValueAsUnsigned()
            self.count = self.valobj.GetValueForExpressionPath(
                ".impl.count").GetValueAsUnsigned()
        except Exception as e:
            print("update error", e)

    def has_children(self):
        return True


class DenseHashSetSyntheticChildrenProvider:
    fixed_names = ["count", "capacity"]
    max_expand_children = 100
    max_expand_capacity = 1000

    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.count = 0
        self.capacity = 0

    def num_children(self):
        return min(self.max_expand_children, self.count) + len(self.fixed_names)

    def get_child_index(self, name):
        try:
            if name in self.fixed_names:
                return self.fixed_names.index(name)

            return -1
        except Exception as e:
            print("get_child_index exception", e, name)
            return -1

    def get_child_at_index(self, index):
        try:
            if index < len(self.fixed_names):
                fixed_name = self.fixed_names[index]
                impl_child = self.valobj.GetValueForExpressionPath(
                    f".impl.{fixed_name}")

                return self.valobj.CreateValueFromData(fixed_name, impl_child.GetData(), impl_child.GetType())
            else:
                index -= len(self.fixed_names)

            empty_key_valobj = self.valobj.GetValueForExpressionPath(
                f".impl.empty_key")
            key_type = empty_key_valobj.GetType().GetCanonicalType().GetName()
            skipped = 0

            for slot in range(0, min(self.max_expand_capacity, self.capacity)):
                slot_valobj = self.valobj.GetValueForExpressionPath(
                    f".impl.data[{slot}]")

                eq_test_valobj = self.valobj.EvaluateExpression(
                    f"*(reinterpret_cast<const {key_type}*>({empty_key_valobj.AddressOf().GetValueAsUnsigned()})) == *(reinterpret_cast<const {key_type}*>({slot_valobj.AddressOf().GetValueAsUnsigned()}))")
                if eq_test_valobj.GetValue() == "true":
                    continue

                # Skip over previous occupied slots.
                if index > skipped:
                    skipped += 1
                    continue

                return self.valobj.CreateValueFromData(f"[{index}]", slot_valobj.GetData(), slot_valobj.GetType())

        except Exception as e:
            print("get_child_at_index error", e, index)

    def update(self):
        try:
            self.capacity = self.count = self.valobj.GetValueForExpressionPath(
                ".impl.capacity").GetValueAsUnsigned()
            self.count = self.valobj.GetValueForExpressionPath(
                ".impl.count").GetValueAsUnsigned()
        except Exception as e:
            print("update error", e)

    def has_children(self):
        return True


def luau_symbol_summary(valobj, internal_dict, options):
    local = valobj.GetChildMemberWithName("local")
    global_ = valobj.GetChildMemberWithName(
        "global").GetChildMemberWithName("value")

    if local.GetValueAsUnsigned() != 0:
        return f'local {local.GetChildMemberWithName("name").GetChildMemberWithName("value").GetSummary()}'
    elif global_.GetValueAsUnsigned() != 0:
        return f"global {global_.GetSummary()}"
    else:
        return "???"


class AstArraySyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj

    def num_children(self):
        return self.size

    def get_child_index(self, name):
        try:
            if name.startswith("[") and name.endswith("]"):
                return int(name[1:-1])
            else:
                return -1
        except Exception as e:
            print("get_child_index error:", e)

    def get_child_at_index(self, index):
        try:
            dataMember = self.valobj.GetChildMemberWithName("data")
            data = dataMember.GetPointeeData(index)
            return self.valobj.CreateValueFromData(
                f"[{index}]", data, dataMember.Dereference().GetType()
            )
        except Exception as e:
            print("get_child_index error:", e)

    def update(self):
        self.size = self.valobj.GetChildMemberWithName(
            "size").GetValueAsUnsigned()

    def has_children(self):
        return True


def luau_typepath_property_summary(valobj, internal_dict, options):
    name = valobj.GetChildMemberWithName("name").GetSummary()
    result = "["

    read_write = False
    try:
        fflag_valobj = valobj.GetFrame().GetValueForVariablePath(
            "FFlag::LuauSolverV2::value")

        read_write = fflag_valobj.GetValue() == "true"
    except Exception as e:
        print("luau_typepath_property_summary error:", e)

    if read_write:
        is_read = valobj.GetChildMemberWithName("isRead").GetValue() == "true"
        if is_read:
            result += "read "
        else:
            result += "write "

    result += name
    result += "]"
    return result

def luau_tstring_summary(valobj, internal_dict):
    str_start = valobj.GetChildMemberWithName("data")
    str_len = valobj.GetChildMemberWithName("len").GetValueAsUnsigned(0)
    str_data = read_non_cstring_from_data(str_start.GetPointeeData(0, str_len))
    return create_quoted_escaped_c_str(str_data)

def tvalue_get_type_name(valobj):
    type_val = valobj.GetChildMemberWithName("tt").GetValueAsUnsigned(0)
    type_map = [
    'TNIL',
    'TBOOLEAN',
    'TLIGHTUSERDATA',
    'TNUMBER',
    'TVECTOR',
    'TSTRING',
    'TTABLE',
    'TFUNCTION',
    'TUSERDATA',
    'TTHREAD',
    'TBUFFER',
    'TPROTO',
    'TUPVAL',
    'TDEADKEY',
    ]

    return f"{type_map[type_val] if type_val < len(type_map) else '<invalid type>'}"

def luau_tvalue_summary(valobj, internal_dict):
    if valobj.GetType().IsPointerType():
        valobj = valobj.Dereference()
    valobj = valobj.GetNonSyntheticValue()

    type_name = tvalue_get_type_name(valobj)

    if type_name == 'TBOOLEAN':
        bool_val = valobj.GetChildMemberWithName("value").GetChildMemberWithName("b").GetValueAsUnsigned(0)
        bool_str = ["false", "true"][bool_val]
        return f"{bool_str} ({type_name})"
    elif type_name == 'TNUMBER':
        num_val = valobj.GetChildMemberWithName("value").GetChildMemberWithName("n")
        return f"{num_val.GetValue()}"
    elif type_name == 'TVECTOR':
        target = valobj.GetTarget()
        float_type = target.GetBasicType(lldb.eBasicTypeFloat)

        x_val = valobj.GetChildMemberWithName("value").GetChildMemberWithName("v").GetChildAtIndex(0).GetValue()
        y_val = valobj.GetChildMemberWithName("value").GetChildMemberWithName("v").GetChildAtIndex(1).GetValue()
        z_val_addr = valobj.GetChildMemberWithName("extra").GetChildAtIndex(0).GetAddress()
        z_val = target.CreateValueFromAddress("z", z_val_addr, float_type).GetValue()
        return f"({x_val}, {y_val}, {z_val}) ({type_name})"
    elif type_name == 'TSTRING':
        ts = valobj.GetChildMemberWithName("value").GetChildMemberWithName("gc").GetChildMemberWithName("ts")
        return f"{ts.GetSummary()}"

    return type_name

class TValueSyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        if valobj.GetType().IsPointerType():
            valobj = valobj.Dereference()
        valobj = valobj.GetNonSyntheticValue()
        
        self.valobj = valobj

    def num_children(self):
        return len(self.children)

    def has_children(self):
        return len(self.children) > 0

    def get_child_at_index(self, index):
        if index < len(self.children):
            return self.children[index]
        return None
    
    def update(self):
        type_name = tvalue_get_type_name(self.valobj)
        if type_name == 'TTABLE':
            luatable = self.valobj.GetChildMemberWithName("value").GetChildMemberWithName("gc").GetChildMemberWithName("h")
            self.children = [luatable.Clone("table")]
        return False
    
def luau_tkey_summary(valobj, internal_dict):
    """TKey has virtually the same layout as TValue, so we can reuse the same summary logic."""
    return luau_tvalue_summary(valobj, internal_dict)

def luau_table_get_entries(valobj):
    """Returns all the valid table entries of a table as two lists. The first list contains the array entries, and the second list contains the hash entries."""
    array_entries = []
    size_array = valobj.GetChildMemberWithName("sizearray").GetValueAsSigned(0)
    array = valobj.GetChildMemberWithName("array")
    array_addr = array.GetValueAsAddress()
    tvalue_type = array.GetType().GetPointeeType()
    tvalue_size = tvalue_type.GetByteSize()
    for i in range(size_array):
        entry = array.CreateValueFromAddress(str(i+1), int(array_addr) + i * tvalue_size, tvalue_type).GetNonSyntheticValue()
        tt = entry.GetChildMemberWithName("tt").GetValueAsUnsigned()
        if tt != 0: # Skip over nil entries.
            array_entries.append(entry)

    hash_entries = []
    size_node = 1 << valobj.GetChildMemberWithName("lsizenode").GetValueAsUnsigned()
    node = valobj.GetChildMemberWithName("node")
    node_addr = node.GetValueAsAddress()
    node_type = node.GetType().GetPointeeType()
    node_size = node_type.GetByteSize()

    for i in range(size_node):
        entry = array.CreateValueFromAddress(f'Node_{i}', int(node_addr) + i * node_size, node_type).GetNonSyntheticValue()
        key = entry.GetChildMemberWithName("key")
        val = entry.GetChildMemberWithName("val").GetNonSyntheticValue()
        tt = val.GetChildMemberWithName("tt").GetValueAsUnsigned()
        if tt != 0: # Skip over entries with nil values.
            hash_entries.append(entry)

    return array_entries, hash_entries

class LuauTableSyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.array_entries = []
        self.hash_entries = []

    def num_children(self):
        return len(self.array_entries) + len(self.hash_entries)

    def has_children(self):
        return True

    def get_child_at_index(self, index):
        array_count = len(self.array_entries)
        if index < array_count:
            return self.array_entries[index]
        else:
            return self.hash_entries[index - array_count]

    def update(self):
        self.array_entries, self.hash_entries = luau_table_get_entries(self.valobj)
        return False

def luau_table_summary(valobj, internal_dict):
    valobj = valobj.GetNonSyntheticValue()
    array_entries, hash_entries = luau_table_get_entries(valobj)
    result = f"LuaTable (size={len(array_entries) + len(hash_entries)})"
    return result

def convert_ptr_size_to_array(name, ptr, num_elem):
    """Converts a SBValue ptr into an array using the name and number of elements provided
       num_elems may be a number of an SBValue with a numeric value. 
    """
    if isinstance(num_elem, lldb.SBValue):
        if num_elem.GetType().GetTypeFlags() & lldb.eTypeIsSigned:
            num_elem = num_elem.GetValueAsSigned()
        else:
            num_elem = num_elem.GetValueAsUnsigned()
    return ptr.CreateValueFromAddress(name, int(ptr.GetValueAsAddress()), ptr.GetType().GetPointeeType().GetArrayType(num_elem))

def read_from_pointer_to_array(ptr, index):
    """ Reads a single element from a pointer to an array. This function is useful because lldb only allows reading
        the 0'th element using GetChildAtIndex for a pointer type.

        ptr should be a SBValue that is a pointer
        index is the index of the array element to read (starting from 0)
    """
    array = convert_ptr_size_to_array('ar', ptr, index+1)
    return array.GetChildAtIndex(index)

def remove_outer_quotes(s):
    return s[1:-1]

def luau_callinfo_summary(valobj, internal_dict):
    func = valobj.GetChildMemberWithName("func").GetNonSyntheticValue()
    cl = func.GetChildMemberWithName("value").GetChildMemberWithName("gc").GetChildMemberWithName("cl")
    isC = cl.GetChildMemberWithName("isC").GetValueAsUnsigned(0) != 0
    if not isC:
        savedpc = valobj.GetChildMemberWithName("savedpc").GetValueAsAddress()
        proto = cl.GetChildMemberWithName("l").GetChildMemberWithName("p")
        code = proto.GetChildMemberWithName("code").GetValueAsAddress()
        linegaplog2 = proto.GetChildMemberWithName("linegaplog2").GetValueAsUnsigned()
        pcRel = 0
        if int(savedpc) != 0:
            pcRel = (int(savedpc) - int(code))//4 - 1
        abslineinfo = proto.GetChildMemberWithName("abslineinfo")
        lineinfo = proto.GetChildMemberWithName("lineinfo")
        source = proto.GetChildMemberWithName("source")
        line = read_from_pointer_to_array(abslineinfo, pcRel >> linegaplog2).GetValueAsUnsigned() + read_from_pointer_to_array(lineinfo, pcRel).GetValueAsUnsigned()
        debugname = proto.GetChildMemberWithName("debugname")
        return f"{remove_outer_quotes(source.GetSummary())}:{line} function {remove_outer_quotes(debugname.GetSummary())}"
    else:
        c = cl.GetChildMemberWithName("c")
        f = c.GetChildMemberWithName("f")
        debugname = c.GetChildMemberWithName("debugname")
        return f"=[C] function {remove_outer_quotes(debugname.GetSummary())} {f.GetSummary()}"

def luau_proto_summary(valobj, internal_dict):
    if valobj.GetType().IsPointerType():
        valobj = valobj.Dereference()
    valobj = valobj.GetNonSyntheticValue()
    source = valobj.GetChildMemberWithName("source")
    debugname = valobj.GetChildMemberWithName("debugname")
    linedefined = valobj.GetChildMemberWithName("linedefined").GetValueAsUnsigned()
    numparams = valobj.GetChildMemberWithName("numparams").GetValueAsUnsigned()
    nups = valobj.GetChildMemberWithName("nups").GetValueAsUnsigned()
    return f'{remove_outer_quotes(source.GetSummary())}:{linedefined} {"function " + remove_outer_quotes(debugname.GetSummary()) if debugname.GetValueAsUnsigned() != 0 else ""} [{numparams} arg, {nups} upval]'

class ProtoSyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        if valobj.GetType().IsPointerType():
           valobj = valobj.Dereference()
        valobj = valobj.GetNonSyntheticValue()
        
        self.valobj = valobj

    def num_children(self):
        return len(self.children)

    def has_children(self):
        return len(self.children) > 0

    def get_child_at_index(self, index):
        if index < len(self.children):
            return self.children[index]
        return None

    def update(self):
        children = []
        self.children = children
        valobj = self.valobj

        k = valobj.GetChildMemberWithName("k")
        sizek = valobj.GetChildMemberWithName("sizek")
        constants_array = convert_ptr_size_to_array("[constants]", k, sizek)
        children.append(constants_array)

        locvars = valobj.GetChildMemberWithName("locvars")
        sizelocvars = valobj.GetChildMemberWithName("sizelocvars")
        locvars_array = convert_ptr_size_to_array("[locvars]", locvars, sizelocvars)
        children.append(locvars_array)

        sizecode = valobj.GetChildMemberWithName("sizecode")
        code = valobj.GetChildMemberWithName("code")
        code_array = convert_ptr_size_to_array("[bytecode]", code, sizecode)
        children.append(code_array)

        sizep = valobj.GetChildMemberWithName("sizep")
        p = valobj.GetChildMemberWithName("p")
        p_array = convert_ptr_size_to_array("[functions]", p, sizep)
        children.append(p_array)

        sizeupvalues = valobj.GetChildMemberWithName("sizeupvalues")
        upvalues = valobj.GetChildMemberWithName("upvalues")
        upvalues_array = convert_ptr_size_to_array("[upvalues]", upvalues, sizeupvalues)
        children.append(upvalues_array)

        children.append(self.valobj.GetChildMemberWithName("source"))
        return False


# Note for future work:
# LLDB is limited in terms of expansion. i.e. a child provider can expand to a set
# of children, but it can't directly express how those children can be expanded further.
# To acheive this functionality for special situations (e.g. showing callstacks in reverse
# order) it may be necessary to create types that are only used for debugging purposes which
# an then define how their children are expanded.
#
# Here's an example of how EvaluateExpression can be used to create such a type on the fly:
# e = lldb.target.EvaluateExpression("struct DebuggerOnlyType{int a; float b;}; (DebuggerOnlyType*)0;")