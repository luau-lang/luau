#ifndef FLOW_MGR_H
#define FLOW_MGR_H
#include <Flow/FlowSng.hpp>

using pre_op_t = bool(*)(std::uint32_t);
using post_op_t = void(*)(std::uint32_t);

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

    bool do_pre_op(std::uint32_t id) {
        return pre_op(id);
    }

    void do_post_op(std::uint32_t id) {
        if (has_post_op())
            post_op(id);
    }


};

#endif // FLOW_MGR_H