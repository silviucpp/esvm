#ifndef C_SRC_ESVM_NIF_H_
#define C_SRC_ESVM_NIF_H_

#include "erl_nif.h"

namespace esvm {

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomBadArg;
};

struct esvm_data
{
    ErlNifResourceType* resEsvmModel;
};

extern atoms ATOMS;

}

#endif  // C_SRC_ESVM_NIF_H_
