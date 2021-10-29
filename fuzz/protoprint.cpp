// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "luau.pb.h"

static const std::string kNames[] = {
    "_G",
    "_LOADED",
    "_VERSION",
    "__add",
    "__call",
    "__concat",
    "__div",
    "__eq",
    "__index",
    "__le",
    "__len",
    "__lt",
    "__mod",
    "__mode",
    "__mul",
    "__namecall",
    "__newindex",
    "__pow",
    "__sub",
    "__type",
    "__unm",
    "abs",
    "acos",
    "arshift",
    "asin",
    "assert",
    "atan",
    "atan2",
    "band",
    "bit32",
    "bnot",
    "boolean",
    "bor",
    "btest",
    "bxor",
    "byte",
    "ceil",
    "char",
    "charpattern",
    "clock",
    "codepoint",
    "codes",
    "concat",
    "coroutine",
    "cos",
    "cosh",
    "create",
    "date",
    "debug",
    "deg",
    "difftime",
    "error",
    "exp",
    "extract",
    "find",
    "floor",
    "fmod",
    "foreach",
    "foreachi",
    "format",
    "frexp",
    "function",
    "gcinfo",
    "getfenv",
    "getinfo",
    "getmetatable",
    "getn",
    "gmatch",
    "gsub",
    "huge",
    "insert",
    "ipairs",
    "isyieldable",
    "ldexp",
    "len",
    "loadstring",
    "log",
    "log10",
    "lower",
    "lrotate",
    "lshift",
    "match",
    "math",
    "max",
    "maxn",
    "min",
    "modf",
    "move",
    "newproxy",
    "next",
    "nil",
    "number",
    "offset",
    "os",
    "pack",
    "packsize",
    "pairs",
    "pcall",
    "pi",
    "pow",
    "print",
    "rad",
    "random",
    "randomseed",
    "rawequal",
    "rawget",
    "rawset",
    "remove",
    "rep",
    "replace",
    "require",
    "resume",
    "reverse",
    "rrotate",
    "rshift",
    "running",
    "select",
    "setfenv",
    "setmetatable",
    "sin",
    "sinh",
    "sort",
    "split",
    "sqrt",
    "status",
    "stdin",
    "string",
    "sub",
    "table",
    "tan",
    "tanh",
    "thread",
    "time",
    "tonumber",
    "tostring",
    "traceback",
    "type",
    "typeof",
    "unpack",
    "upper",
    "userdata",
    "utf8",
    "vector",
    "wrap",
    "xpcall",
    "yield",
};

static const std::string kTypes[] = {
    "any",
    "nil",
    "number",
    "string",
    "boolean",
    "thread",
};

static const std::string kClasses[] = {
    "Vector3",
    "Instance",
    "Part",
};

struct ProtoToLuau
{
    struct Function
    {
        int loops = 0;
        bool vararg = false;
    };

    std::string source;
    std::vector<Function> functions;
    bool types = false;

    ProtoToLuau()
    {
        Function top = {};
        top.vararg = true;
        functions.push_back(top);
    }

    void ident(const luau::Name& name)
    {
        if (name.has_builtin())
        {
            size_t index = size_t(name.builtin()) % std::size(kNames);
            source += kNames[index];
        }
        else if (name.has_custom())
        {
            source += 'n';
            source += std::to_string(name.custom() & 0xff);
        }
        else
        {
            source += '_';
        }
    }

    void ident(const luau::Typename& name)
    {
        source += 't';
        source += std::to_string(name.index() & 0xff);
    }

    void print(const luau::Expr& expr)
    {
        if (expr.has_group())
            print(expr.group());
        else if (expr.has_nil())
            print(expr.nil());
        else if (expr.has_bool_())
            print(expr.bool_());
        else if (expr.has_number())
            print(expr.number());
        else if (expr.has_string())
            print(expr.string());
        else if (expr.has_local())
            print(expr.local());
        else if (expr.has_global())
            print(expr.global());
        else if (expr.has_varargs())
            print(expr.varargs());
        else if (expr.has_call())
            print(expr.call());
        else if (expr.has_index_name())
            print(expr.index_name());
        else if (expr.has_index_expr())
            print(expr.index_expr());
        else if (expr.has_function())
            print(expr.function());
        else if (expr.has_table())
            print(expr.table());
        else if (expr.has_unary())
            print(expr.unary());
        else if (expr.has_binary())
            print(expr.binary());
        else
            source += "_";
    }

    void print(const luau::ExprPrefix& expr)
    {
        if (expr.has_group())
            print(expr.group());
        else if (expr.has_local())
            print(expr.local());
        else if (expr.has_global())
            print(expr.global());
        else if (expr.has_call())
            print(expr.call());
        else if (expr.has_index_name())
            print(expr.index_name());
        else if (expr.has_index_expr())
            print(expr.index_expr());
        else
            source += "_";
    }

    void print(const luau::ExprGroup& expr)
    {
        source += '(';
        print(expr.expr());
        source += ')';
    }

    void print(const luau::ExprConstantNil& expr)
    {
        source += "nil";
    }

    void print(const luau::ExprConstantBool& expr)
    {
        source += expr.val() ? "true" : "false";
    }

    void print(const luau::ExprConstantNumber& expr)
    {
        source += std::to_string(expr.val());
    }

    void print(const luau::ExprConstantString& expr)
    {
        source += '"';
        for (char ch : expr.val())
            if (isalpha(ch))
                source += ch;
        source += '"';
    }

    void print(const luau::Local& var)
    {
        source += 'l';
        source += std::to_string(var.name() & 0xff);
    }

    void print(const luau::ExprLocal& expr)
    {
        print(expr.var());
    }

    void print(const luau::ExprGlobal& expr)
    {
        ident(expr.name());
    }

    void print(const luau::ExprVarargs& expr)
    {
        if (functions.back().vararg)
            source += "...";
        else
            source += "_";
    }

    void print(const luau::ExprCall& expr)
    {
        if (expr.func().has_index_name())
            print(expr.func().index_name(), expr.self());
        else
            print(expr.func());
        source += '(';
        for (int i = 0; i < expr.args_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(expr.args(i));
        }
        source += ')';
    }

    void print(const luau::ExprIndexName& expr, bool self = false)
    {
        print(expr.expr());
        source += self ? ':' : '.';
        ident(expr.index());
    }

    void print(const luau::ExprIndexExpr& expr)
    {
        print(expr.expr());
        source += '[';
        print(expr.index());
        source += ']';
    }

    void function(const luau::ExprFunction& expr)
    {
        source += "(";
        for (int i = 0; i < expr.args_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(expr.args(i));

            if (types && i < expr.types_size())
            {
                source += ':';
                print(expr.types(i));
            }
        }
        if (expr.vararg())
        {
            if (expr.args_size())
                source += ',';
            source += "...";
        }
        source += ')';
        if (types && expr.rettypes_size())
        {
            source += ':';
            if (expr.rettypes_size() > 1)
                source += '(';
            for (size_t i = 0; i < expr.rettypes_size(); ++i)
            {
                if (i != 0)
                    source += ',';
                print(expr.rettypes(i));
            }
            if (expr.rettypes_size() > 1)
                source += ')';
        }
        source += '\n';

        Function func = {};
        func.vararg = expr.vararg();
        functions.push_back(func);

        print(expr.body());

        functions.pop_back();

        source += "end";
    }

    void print(const luau::ExprFunction& expr)
    {
        source += "function";
        function(expr);
    }

    void print(const luau::ExprTable& expr)
    {
        source += '{';
        for (int i = 0; i < expr.items_size(); ++i)
        {
            if (expr.items(i).has_key_name())
            {
                ident(expr.items(i).key_name());
                source += '=';
            }
            else if (expr.items(i).has_key_expr())
            {
                source += "[";
                print(expr.items(i).key_expr());
                source += "]=";
            }

            print(expr.items(i).value());
            source += ',';
        }
        source += '}';
    }

    void print(const luau::ExprUnary& expr)
    {
        if (expr.op() == luau::ExprUnary::Not)
            source += "not ";
        else if (expr.op() == luau::ExprUnary::Minus)
            source += "- ";
        else if (expr.op() == luau::ExprUnary::Len)
            source += "# ";

        print(expr.expr());
    }

    void print(const luau::ExprBinary& expr)
    {
        print(expr.left());

        if (expr.op() == luau::ExprBinary::Add)
            source += " + ";
        else if (expr.op() == luau::ExprBinary::Sub)
            source += " - ";
        else if (expr.op() == luau::ExprBinary::Mul)
            source += " * ";
        else if (expr.op() == luau::ExprBinary::Div)
            source += " / ";
        else if (expr.op() == luau::ExprBinary::Mod)
            source += " % ";
        else if (expr.op() == luau::ExprBinary::Pow)
            source += " ^ ";
        else if (expr.op() == luau::ExprBinary::Concat)
            source += " .. ";
        else if (expr.op() == luau::ExprBinary::CompareNe)
            source += " ~= ";
        else if (expr.op() == luau::ExprBinary::CompareEq)
            source += " == ";
        else if (expr.op() == luau::ExprBinary::CompareLt)
            source += " < ";
        else if (expr.op() == luau::ExprBinary::CompareLe)
            source += " <= ";
        else if (expr.op() == luau::ExprBinary::CompareGt)
            source += " > ";
        else if (expr.op() == luau::ExprBinary::CompareGe)
            source += " >= ";
        else if (expr.op() == luau::ExprBinary::And)
            source += " and ";
        else if (expr.op() == luau::ExprBinary::Or)
            source += " or ";

        print(expr.right());
    }

    void print(const luau::LValue& expr)
    {
        if (expr.has_local())
            print(expr.local());
        else if (expr.has_global())
            print(expr.global());
        else if (expr.has_index_name())
            print(expr.index_name());
        else if (expr.has_index_expr())
            print(expr.index_expr());
        else
            source += "_";
    }

    void print(const luau::Stat& stat)
    {
        if (stat.has_block())
            print(stat.block());
        else if (stat.has_if_())
            print(stat.if_());
        else if (stat.has_while_())
            print(stat.while_());
        else if (stat.has_repeat())
            print(stat.repeat());
        else if (stat.has_break_())
            print(stat.break_());
        else if (stat.has_continue_())
            print(stat.continue_());
        else if (stat.has_return_())
            print(stat.return_());
        else if (stat.has_call())
            print(stat.call());
        else if (stat.has_local())
            print(stat.local());
        else if (stat.has_for_())
            print(stat.for_());
        else if (stat.has_for_in())
            print(stat.for_in());
        else if (stat.has_assign())
            print(stat.assign());
        else if (stat.has_compound_assign())
            print(stat.compound_assign());
        else if (stat.has_function())
            print(stat.function());
        else if (stat.has_local_function())
            print(stat.local_function());
        else if (stat.has_type_alias())
            print(stat.type_alias());
        else
            source += "do end\n";
    }

    void print(const luau::StatBlock& stat)
    {
        for (int i = 0; i < stat.body_size(); ++i)
        {
            if (stat.body(i).has_block())
            {
                source += "do\n";
                print(stat.body(i));
                source += "end\n";
            }
            else
            {
                print(stat.body(i));

                // parser will reject code with break/continue/return being non-trailing statements in a block
                if (stat.body(i).has_break_() || stat.body(i).has_continue_() || stat.body(i).has_return_())
                    break;
            }
        }
    }

    void print(const luau::StatIf& stat)
    {
        source += "if ";
        print(stat.cond());
        source += " then\n";
        print(stat.then());

        if (stat.has_else_())
        {
            source += "else\n";
            print(stat.else_());
            source += "end\n";
        }
        else if (stat.has_elseif())
        {
            source += "else";
            print(stat.elseif());
        }
        else
        {
            source += "end\n";
        }
    }

    void print(const luau::StatWhile& stat)
    {
        source += "while ";
        print(stat.cond());
        source += " do\n";

        functions.back().loops++;
        print(stat.body());
        functions.back().loops--;

        source += "end\n";
    }

    void print(const luau::StatRepeat& stat)
    {
        source += "repeat\n";

        functions.back().loops++;
        print(stat.body());
        functions.back().loops--;

        source += "until ";
        print(stat.cond());
        source += "\n";
    }

    void print(const luau::StatBreak& stat)
    {
        if (functions.back().loops)
            source += "break\n";
        else
            source += "do end\n";
    }

    void print(const luau::StatContinue& stat)
    {
        if (functions.back().loops)
            source += "continue\n";
        else
            source += "do end\n";
    }

    void print(const luau::StatReturn& stat)
    {
        source += "return ";
        for (int i = 0; i < stat.list_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(stat.list(i));
        }
        source += "\n";
    }

    void print(const luau::StatCall& stat)
    {
        print(stat.expr());
        source += '\n';
    }

    void print(const luau::StatLocal& stat)
    {
        source += "local ";

        if (stat.vars_size() == 0)
            source += '_';

        for (int i = 0; i < stat.vars_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(stat.vars(i));

            if (types && i < stat.types_size())
            {
                source += ':';
                print(stat.types(i));
            }
        }

        if (stat.values_size() != 0)
            source += " = ";

        for (int i = 0; i < stat.values_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(stat.values(i));
        }
        source += '\n';
    }

    void print(const luau::StatFor& stat)
    {
        source += "for ";
        print(stat.var());
        source += '=';
        print(stat.from());
        source += ',';
        print(stat.to());
        if (stat.has_step())
        {
            source += ',';
            print(stat.step());
        }
        source += " do\n";

        functions.back().loops++;
        print(stat.body());
        functions.back().loops--;

        source += "end\n";
    }

    void print(const luau::StatForIn& stat)
    {
        source += "for ";

        if (stat.vars_size() == 0)
            source += '_';

        for (int i = 0; i < stat.vars_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(stat.vars(i));
        }

        source += " in ";

        if (stat.values_size() == 0)
            source += "...";

        for (int i = 0; i < stat.values_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(stat.values(i));
        }

        source += " do\n";

        functions.back().loops++;
        print(stat.body());
        functions.back().loops--;

        source += "end\n";
    }

    void print(const luau::StatAssign& stat)
    {
        if (stat.vars_size() == 0)
            source += '_';

        for (int i = 0; i < stat.vars_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(stat.vars(i));
        }

        source += " = ";

        if (stat.values_size() == 0)
            source += "nil";

        for (int i = 0; i < stat.values_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(stat.values(i));
        }
        source += '\n';
    }

    void print(const luau::StatCompoundAssign& stat)
    {
        print(stat.var());

        if (stat.op() == luau::StatCompoundAssign::Add)
            source += " += ";
        else if (stat.op() == luau::StatCompoundAssign::Sub)
            source += " -= ";
        else if (stat.op() == luau::StatCompoundAssign::Mul)
            source += " *= ";
        else if (stat.op() == luau::StatCompoundAssign::Div)
            source += " /= ";
        else if (stat.op() == luau::StatCompoundAssign::Mod)
            source += " %= ";
        else if (stat.op() == luau::StatCompoundAssign::Pow)
            source += " ^= ";
        else if (stat.op() == luau::StatCompoundAssign::Concat)
            source += " ..= ";

        print(stat.value());
        source += '\n';
    }

    void print(const luau::StatFunction& stat)
    {
        source += "function ";
        if (stat.var().has_index_name())
            print(stat.var().index_name(), stat.self());
        else if (stat.var().has_index_expr())
            source += '_'; // function foo[bar]() is invalid syntax
        else
            print(stat.var());
        function(stat.func());
        source += '\n';
    }

    void print(const luau::StatLocalFunction& stat)
    {
        source += "local function ";
        print(stat.var());
        function(stat.func());
        source += '\n';
    }

    void print(const luau::StatTypeAlias& stat)
    {
        source += "type ";
        ident(stat.name());

        if (stat.generics_size())
        {
            source += '<';
            for (size_t i = 0; i < stat.generics_size(); ++i)
            {
                if (i != 0)
                    source += ',';
                ident(stat.generics(i));
            }
            source += '>';
        }

        source += " = ";
        print(stat.type());
        source += '\n';
    }

    void print(const luau::Type& type)
    {
        if (type.has_primitive())
            print(type.primitive());
        else if (type.has_literal())
            print(type.literal());
        else if (type.has_table())
            print(type.table());
        else if (type.has_function())
            print(type.function());
        else if (type.has_typeof())
            print(type.typeof());
        else if (type.has_union_())
            print(type.union_());
        else if (type.has_intersection())
            print(type.intersection());
        else if (type.has_class_())
            print(type.class_());
        else if (type.has_ref())
            print(type.ref());
        else
            source += "any";
    }

    void print(const luau::TypePrimitive& type)
    {
        size_t index = size_t(type.kind()) % std::size(kTypes);
        source += kTypes[index];
    }

    void print(const luau::TypeLiteral& type)
    {
        ident(type.name());

        if (type.generics_size())
        {
            source += '<';
            for (size_t i = 0; i < type.generics_size(); ++i)
            {
                if (i != 0)
                    source += ',';
                ident(type.generics(i));
            }
            source += '>';
        }
    }

    void print(const luau::TypeTable& type)
    {
        source += '{';
        for (size_t i = 0; i < type.items_size(); ++i)
        {
            ident(type.items(i).key());
            source += ':';
            print(type.items(i).type());
            source += ',';
        }
        if (type.has_indexer())
        {
            source += '[';
            print(type.indexer().key());
            source += "]:";
            print(type.indexer().value());
        }
        source += '}';
    }

    void print(const luau::TypeFunction& type)
    {
        source += '(';
        for (size_t i = 0; i < type.args_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(type.args(i));
        }
        source += ")->";
        if (type.rets_size() != 1)
            source += '(';
        for (size_t i = 0; i < type.rets_size(); ++i)
        {
            if (i != 0)
                source += ',';
            print(type.rets(i));
        }
        if (type.rets_size() != 1)
            source += ')';
    }

    void print(const luau::TypeTypeof& type)
    {
        source += "typeof(";
        print(type.expr());
        source += ')';
    }

    void print(const luau::TypeUnion& type)
    {
        source += '(';
        print(type.left());
        source += ")|(";
        print(type.right());
        source += ')';
    }

    void print(const luau::TypeIntersection& type)
    {
        source += '(';
        print(type.left());
        source += ")&(";
        print(type.right());
        source += ')';
    }

    void print(const luau::TypeClass& type)
    {
        size_t index = size_t(type.kind()) % std::size(kClasses);
        source += kClasses[index];
    }

    void print(const luau::TypeRef& type)
    {
        print(type.prefix());
        source += '.';
        ident(type.index());
    }
};

std::string protoprint(const luau::StatBlock& stat, bool types)
{
    ProtoToLuau printer;
    printer.types = types;
    printer.print(stat);
    return printer.source;
}
