#ifndef C_SRC_NIF_UTILS_H_
#define C_SRC_NIF_UTILS_H_

#include <stdint.h>
#include <string>

#include "erl_nif.h"

namespace esvm {

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM make_error(ErlNifEnv* env, const char* error);
ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM term);
ERL_NIF_TERM make_badarg(ErlNifEnv* env);
ERL_NIF_TERM make_binary(ErlNifEnv* env, const char* buff, size_t length);
ERL_NIF_TERM make_ok_result(ErlNifEnv* env, ERL_NIF_TERM term);

bool get_binary(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary* bin);
bool get_string(ErlNifEnv *env, ERL_NIF_TERM term, std::string* var);
bool get_boolean(ERL_NIF_TERM term, bool* val);

bool get_double(ErlNifEnv* env, ERL_NIF_TERM term, double *d);

}

#endif  // C_SRC_NIF_UTILS_H_
