# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

class VariantPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        typeId = int(self.val['typeId'])
        type = self.val.type.template_argument(typeId)
        value = self.val['storage'].reinterpret_cast(type.pointer()).dereference()
        return type.name + " [" + str(value) + "]"

def match_printer(val):
	type = val.type.strip_typedefs()
	if type.name and type.name.startswith('Luau::Variant<'):
		return VariantPrinter(val)
	return None

gdb.pretty_printers.append(match_printer)
