#ifndef FLOW_MGR_H
#define FLOW_MGR_H
#include <Flow/FlowSng.hpp>

struct lua_State;

using pre_op_t = bool(*)(lua_State*,std::uint32_t*);
using post_op_t = void(*)(lua_State*, std::uint32_t*);

class Flow : public Singleton<Flow> {
    pre_op_t pre_op;
    post_op_t post_op;
public:
    void set_pre_op(pre_op_t op) {
        pre_op = op;
    }

        bool has_pre_op() {
        return pre_op != nullptr;
    }

    bool has_post_op() {
        return post_op != nullptr;
    }

    void set_post_op(post_op_t op) {
        post_op = op;
    }

    bool do_pre_op(lua_State* L, std::uint32_t* pc) {
        if (has_pre_op())
            return pre_op(L, pc);
        return true; // pass by default
    }

    void do_post_op(lua_State* L, std::uint32_t* pc) {
        if (has_post_op())
            post_op(L, pc);
    }


};

#endif // FLOW_MGR_H