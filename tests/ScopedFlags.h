// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Common.h"

#include <string.h>

template<typename T>
struct ScopedFValue
{
private:
    lluz::FValue<T>* value = nullptr;
    T oldValue = T();

public:
    ScopedFValue(const char* name, T newValue)
    {
        for (lluz::FValue<T>* v = lluz::FValue<T>::list; v; v = v->next)
            if (strcmp(v->name, name) == 0)
            {
                value = v;
                oldValue = v->value;
                v->value = newValue;
                break;
            }

        lluz_ASSERT(value);
    }

    ScopedFValue(const ScopedFValue&) = delete;
    ScopedFValue& operator=(const ScopedFValue&) = delete;

    ScopedFValue(ScopedFValue&& rhs)
    {
        value = rhs.value;
        oldValue = rhs.oldValue;

        rhs.value = nullptr;
    }

    ScopedFValue& operator=(ScopedFValue&& rhs)
    {
        value = rhs.value;
        oldValue = rhs.oldValue;

        rhs.value = nullptr;

        return *this;
    }

    ~ScopedFValue()
    {
        if (value)
            value->value = oldValue;
    }
};

using ScopedFastFlag = ScopedFValue<bool>;
using ScopedFastInt = ScopedFValue<int>;
