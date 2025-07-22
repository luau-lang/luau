// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "Luau/ExperimentalFlags.h"

#include <string_view>

#include <stdio.h>
#include <string.h>

static void setLuauFlag(std::string_view name, bool state)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (name == flag->name)
        {
            flag->value = state;
            return;
        }
    }

    fprintf(stderr, "Warning: unrecognized flag '%.*s'.\n", int(name.length()), name.data());
}

static void setLuauFlags(bool state)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = state;
}

void setLuauFlagsDefault()
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0 && !Luau::isAnalysisFlagExperimental(flag->name))
            flag->value = true;
}

void setLuauFlags(const char* list)
{
    std::string_view rest = list;

    while (!rest.empty())
    {
        size_t ending = rest.find(",");
        std::string_view element = rest.substr(0, ending);

        if (size_t separator = element.find('='); separator != std::string_view::npos)
        {
            std::string_view key = element.substr(0, separator);
            std::string_view value = element.substr(separator + 1);

            if (value == "true" || value == "True")
                setLuauFlag(key, true);
            else if (value == "false" || value == "False")
                setLuauFlag(key, false);
            else
                fprintf(
                    stderr, "Warning: unrecognized value '%.*s' for flag '%.*s'.\n", int(value.length()), value.data(), int(key.length()), key.data()
                );
        }
        else
        {
            if (element == "true" || element == "True")
                setLuauFlags(true);
            else if (element == "false" || element == "False")
                setLuauFlags(false);
            else
                setLuauFlag(element, true);
        }

        if (ending != std::string_view::npos)
            rest.remove_prefix(ending + 1);
        else
            break;
    }
}
