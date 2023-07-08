#ifndef C_SRC_ESVM_H_
#define C_SRC_ESVM_H_

#include "erl_nif.h"

namespace esvm {

void nif_svm_model_free(ErlNifEnv* env, void* obj);
ERL_NIF_TERM nif_svm_model_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_svm_model_load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_svm_model_save(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_svm_model_predict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

}

#endif  // C_SRC_ESVM_H_
