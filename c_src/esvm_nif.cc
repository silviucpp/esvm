#include "esvm_nif.h"
#include "nif_utils.h"
#include "macros.h"
#include "esvm.h"

#include <stdlib.h>

namespace esvm {

namespace {

const char kAtomOk[] = "ok";
const char kAtomError[] = "error";
const char kAtomTrue[] = "true";
const char kAtomFalse[] = "false";
const char kAtomBadArg[] = "badarg";

static ErlNifFunc nif_funcs[] = {
   {"model_create", 3, nif_svm_model_create, ERL_NIF_DIRTY_JOB_IO_BOUND},
   {"model_load", 1, nif_svm_model_load, ERL_NIF_DIRTY_JOB_IO_BOUND},
   {"model_save", 2, nif_svm_model_save, ERL_NIF_DIRTY_JOB_IO_BOUND},
   {"model_predict", 2, nif_svm_model_predict, ERL_NIF_DIRTY_JOB_IO_BOUND}
};

void open_resources(ErlNifEnv* env, esvm_data* data)
{
    ErlNifResourceFlags flags =  static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    data->resEsvmModel = enif_open_resource_type(env, NULL, "enif_svm_model", nif_svm_model_free, flags, NULL);
}

}

atoms ATOMS;

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomOk = make_atom(env, kAtomOk);
    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomTrue = make_atom(env, kAtomTrue);
    ATOMS.atomFalse = make_atom(env, kAtomFalse);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);

    esvm_data* data = static_cast<esvm_data*>(enif_alloc(sizeof(esvm_data)));
    open_resources(env, data);
    *priv_data = data;
    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    esvm_data* data = static_cast<esvm_data*>(priv_data);
    enif_free(data);
}

int on_nif_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    UNUSED(old_priv);
    UNUSED(info);

    esvm_data* data = static_cast<esvm_data*>(enif_alloc(sizeof(esvm_data)));
    open_resources(env, data);
    *priv = data;
    return 0;
}

}

ERL_NIF_INIT(esvm_nif, esvm::nif_funcs, esvm::on_nif_load, NULL, esvm::on_nif_upgrade, esvm::on_nif_unload)
