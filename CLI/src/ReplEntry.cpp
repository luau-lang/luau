// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Repl.h"
#include "Luau/Flags.h"

int main(int argc, char** argv)
{
    setLuauFlagsDefault();

    return replMain(argc, argv);
}
