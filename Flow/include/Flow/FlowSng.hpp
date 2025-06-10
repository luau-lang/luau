#ifndef FLOW_SNG_H
#define FLOW_SNG_H


#include <memory>
#include <mutex>

template <typename T>
class Singleton {
public:
    // Accessor for the singleton instance
    static T& getInstance() {
        std::call_once(initFlag(), []() {
            instance().reset(new T());
        });
        return *instance();
    }

    // Delete copy and move operations
    Singleton(const Singleton&) = delete;
    Singleton& operator=(const Singleton&) = delete;
    Singleton(Singleton&&) = delete;
    Singleton& operator=(Singleton&&) = delete;

protected:
    Singleton() = default;
    virtual ~Singleton() = default;

private:
    static std::unique_ptr<T>& instance() {
        static std::unique_ptr<T> inst;
        return inst;
    }

    static std::once_flag& initFlag() {
        static std::once_flag flag;
        return flag;
    }
};


#endif // FLOW_SNG_H