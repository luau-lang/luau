// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/DenseHash.h"
#include "Luau/StringUtils.h"

#include <string>

namespace Luau
{

class BytecodeEncoder
{
public:
    virtual ~BytecodeEncoder() {}

    virtual void encode(uint32_t* data, size_t count) = 0;
};

class BytecodeBuilder
{
public:
    // BytecodeBuilder does *not* copy the data passed via StringRef; instead, it keeps the ref around until finalize()
    // Please be careful with the lifetime of the data that's being passed because of this.
    // The safe and correct pattern is to only build StringRefs out of pieces of AST (AstName or AstArray<>) that are backed by AstAllocator.
    // Note that you must finalize() the builder before the Allocator backing the Ast is destroyed.
    struct StringRef
    {
        // To construct a StringRef, use sref() from Compiler.cpp.
        const char* data = nullptr;
        size_t length = 0;

        bool operator==(const StringRef& other) const;
    };

    struct TableShape
    {
        static const unsigned int kMaxLength = 32;

        int32_t keys[kMaxLength];
        unsigned int length = 0;

        bool operator==(const TableShape& other) const;
    };

    BytecodeBuilder(BytecodeEncoder* encoder = 0);

    uint32_t beginFunction(uint8_t numparams, bool isvararg = false);
    void endFunction(uint8_t maxstacksize, uint8_t numupvalues, uint8_t flags = 0);

    void setMainFunction(uint32_t fid);

    int32_t addConstantNil();
    int32_t addConstantBoolean(bool value);
    int32_t addConstantNumber(double value);
    int32_t addConstantString(StringRef value);
    int32_t addImport(uint32_t iid);
    int32_t addConstantTable(const TableShape& shape);
    int32_t addConstantClosure(uint32_t fid);

    int16_t addChildFunction(uint32_t fid);

    void emitABC(LuauOpcode op, uint8_t a, uint8_t b, uint8_t c);
    void emitAD(LuauOpcode op, uint8_t a, int16_t d);
    void emitE(LuauOpcode op, int32_t e);
    void emitAux(uint32_t aux);

    size_t emitLabel();

    [[nodiscard]] bool patchJumpD(size_t jumpLabel, size_t targetLabel);
    [[nodiscard]] bool patchSkipC(size_t jumpLabel, size_t targetLabel);

    void foldJumps();
    void expandJumps();

    void setFunctionTypeInfo(std::string value);

    void setDebugFunctionName(StringRef name);
    void setDebugFunctionLineDefined(int line);
    void setDebugLine(int line);
    void pushDebugLocal(StringRef name, uint8_t reg, uint32_t startpc, uint32_t endpc);
    void pushDebugUpval(StringRef name);

    size_t getInstructionCount() const;
    uint32_t getDebugPC() const;

    void addDebugRemark(const char* format, ...) LUAU_PRINTF_ATTR(2, 3);

    void finalize();

    enum DumpFlags
    {
        Dump_Code = 1 << 0,
        Dump_Lines = 1 << 1,
        Dump_Source = 1 << 2,
        Dump_Locals = 1 << 3,
        Dump_Remarks = 1 << 4,
    };

    void setDumpFlags(uint32_t flags)
    {
        dumpFlags = flags;
        dumpFunctionPtr = &BytecodeBuilder::dumpCurrentFunction;
    }

    void setDumpSource(const std::string& source);

    bool needsDebugRemarks() const
    {
        return (dumpFlags & Dump_Remarks) != 0;
    }

    const std::string& getBytecode() const
    {
        LUAU_ASSERT(!bytecode.empty()); // did you forget to call finalize?
        return bytecode;
    }

    std::string dumpFunction(uint32_t id) const;
    std::string dumpEverything() const;
    std::string dumpSourceRemarks() const;
    std::string dumpTypeInfo() const;

    void annotateInstruction(std::string& result, uint32_t fid, uint32_t instpos) const;

    static uint32_t getImportId(int32_t id0);
    static uint32_t getImportId(int32_t id0, int32_t id1);
    static uint32_t getImportId(int32_t id0, int32_t id1, int32_t id2);

    static int decomposeImportId(uint32_t ids, int32_t& id0, int32_t& id1, int32_t& id2);

    static uint32_t getStringHash(StringRef key);

    static std::string getError(const std::string& message);

    static uint8_t getVersion();
    static uint8_t getTypeEncodingVersion();

private:
    struct Constant
    {
        enum Type
        {
            Type_Nil,
            Type_Boolean,
            Type_Number,
            Type_String,
            Type_Import,
            Type_Table,
            Type_Closure,
        };

        Type type;
        union
        {
            bool valueBoolean;
            double valueNumber;
            unsigned int valueString; // index into string table
            uint32_t valueImport;     // 10-10-10-2 encoded import id
            uint32_t valueTable;      // index into tableShapes[]
            uint32_t valueClosure;    // index of function in global list
        };
    };

    struct ConstantKey
    {
        Constant::Type type;
        // Note: this stores value* from Constant; when type is Number_Double, this stores the same bits as double does but in uint64_t.
        uint64_t value;

        bool operator==(const ConstantKey& key) const
        {
            return type == key.type && value == key.value;
        }
    };

    struct Function
    {
        std::string data;

        uint8_t maxstacksize = 0;
        uint8_t numparams = 0;
        uint8_t numupvalues = 0;
        bool isvararg = false;

        unsigned int debugname = 0;
        int debuglinedefined = 0;

        std::string dump;
        std::string dumpname;
        std::vector<int> dumpinstoffs;
        std::string typeinfo;
    };

    struct DebugLocal
    {
        unsigned int name;

        uint8_t reg;
        uint32_t startpc;
        uint32_t endpc;
    };

    struct DebugUpval
    {
        unsigned int name;
    };

    struct Jump
    {
        uint32_t source;
        uint32_t target;
    };

    struct StringRefHash
    {
        size_t operator()(const StringRef& v) const;
    };

    struct ConstantKeyHash
    {
        size_t operator()(const ConstantKey& key) const;
    };

    struct TableShapeHash
    {
        size_t operator()(const TableShape& v) const;
    };

    std::vector<Function> functions;
    uint32_t currentFunction = ~0u;
    uint32_t mainFunction = ~0u;

    std::vector<uint32_t> insns;
    std::vector<int> lines;
    std::vector<Constant> constants;
    std::vector<uint32_t> protos;
    std::vector<Jump> jumps;

    std::vector<TableShape> tableShapes;

    bool hasLongJumps = false;

    DenseHashMap<ConstantKey, int32_t, ConstantKeyHash> constantMap;
    DenseHashMap<TableShape, int32_t, TableShapeHash> tableShapeMap;
    DenseHashMap<uint32_t, int16_t> protoMap;

    int debugLine = 0;

    std::vector<DebugLocal> debugLocals;
    std::vector<DebugUpval> debugUpvals;

    DenseHashMap<StringRef, unsigned int, StringRefHash> stringTable;
    std::vector<StringRef> debugStrings;

    std::vector<std::pair<uint32_t, uint32_t>> debugRemarks;
    std::string debugRemarkBuffer;

    BytecodeEncoder* encoder = nullptr;
    std::string bytecode;

    uint32_t dumpFlags = 0;
    std::vector<std::string> dumpSource;
    std::vector<std::pair<int, std::string>> dumpRemarks;

    std::string (BytecodeBuilder::*dumpFunctionPtr)(std::vector<int>&) const = nullptr;

    void validate() const;
    void validateInstructions() const;
    void validateVariadic() const;

    std::string dumpCurrentFunction(std::vector<int>& dumpinstoffs) const;
    void dumpConstant(std::string& result, int k) const;
    void dumpInstruction(const uint32_t* opcode, std::string& output, int targetLabel) const;

    void writeFunction(std::string& ss, uint32_t id, uint8_t flags) const;
    void writeLineInfo(std::string& ss) const;
    void writeStringTable(std::string& ss) const;

    int32_t addConstant(const ConstantKey& key, const Constant& value);
    unsigned int addStringTableEntry(StringRef value);
};

} // namespace Luau
