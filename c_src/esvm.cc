#include "esvm.h"
#include "esvm_nif.h"
#include "nif_utils.h"
#include "macros.h"
#include "svm.h"

#include <vector>
#include <set>

typedef std::vector<svm_node> FeatureVector;

namespace esvm {

namespace {

const char kFailedToAllocResourceMsg[] = "enif_alloc_resource failed";

struct enif_svm_model
{
    svm_model* model_;
    svm_problem* prob_;
};

void init_params(svm_parameter* param)
{
    param->svm_type = C_SVC;
    param->kernel_type = RBF;
    param->degree = 3;
    param->gamma = 0.5;
    param->coef0 = 0;
    param->nu = 0.5;
    param->cache_size = 100;
    param->C = 1;
    param->eps = 1e-3;
    param->p = 0.1;
    param->shrinking = 1;
    param->probability = 0;
    param->nr_weight = 0;
    param->weight_label = NULL;
    param->weight = NULL;
}

ERL_NIF_TERM parse_params(ErlNifEnv* env, ERL_NIF_TERM list, svm_parameter* params, bool* has_gamma)
{
    if(!enif_is_list(env, list))
            return false;

    ERL_NIF_TERM head;
    const ERL_NIF_TERM *items;
    int arity;

    std::string key;
    int int_value;
    double double_value;

    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return make_badarg(env);

        if(!get_string(env, items[0], &key))
            return make_badarg(env);

        if(key == "svm_type")
        {
             if(!enif_get_int(env, items[1], &int_value))
                 return make_error(env, "invalid 'svm_type' value");

             params->svm_type = int_value;
        }
        else if(key == "kernel_type")
        {
            if(!enif_get_int(env, items[1], &int_value))
                return make_error(env, "invalid 'kernel_type' value");

            params->kernel_type = int_value;
        }
        else if(key == "degree")
        {
            if(!enif_get_int(env, items[1], &int_value))
                return make_error(env, "invalid 'degree' value");

            params->degree = int_value;
        }
        else if(key == "gamma")
        {
            if(!get_double(env, items[1], &double_value))
                return make_error(env, "invalid 'gamma' value");

            params->gamma = double_value;
            *has_gamma = true;
        }
        else if(key == "coef0")
        {
            if(!get_double(env, items[1], &double_value))
                return make_error(env, "invalid 'coef0' value");

            params->coef0 = double_value;
        }
        else if(key == "cache_size")
        {
            if(!get_double(env, items[1], &double_value))
                return make_error(env, "invalid 'cache_size' value");

            params->cache_size = double_value;
        }
        else if(key == "eps")
        {
            if(!get_double(env, items[1], &double_value))
                return make_error(env, "invalid 'eps' value");

            params->eps = double_value;
        }
        else if(key == "C")
        {
            if(!get_double(env, items[1], &double_value))
                return make_error(env, "invalid 'C' value");

            params->C = double_value;
        }
        else if(key == "nu")
        {
            if(!get_double(env, items[1], &double_value))
                return make_error(env, "invalid 'nu' value");

            params->nu = double_value;
        }
        else if(key == "p")
        {
            if(!get_double(env, items[1], &double_value))
                return make_error(env, "invalid 'p' value");

            params->p = double_value;
        }
        else if(key == "shrinking")
        {
            if(!enif_get_int(env, items[1], &int_value))
                return make_error(env, "invalid 'shrinking' value");

            params->shrinking = int_value;
        }
        else if(key == "probability")
        {
            if(!enif_get_int(env, items[1], &int_value))
                return make_error(env, "invalid 'probability' value");

            params->probability = int_value;
        }
    }
    return ATOMS.atomOk;
}

bool parse_feature_vector(ErlNifEnv* env, ERL_NIF_TERM list, FeatureVector& feature_vector)
{
    if(!enif_is_list(env, list))
        return false;

    ERL_NIF_TERM head;
    const ERL_NIF_TERM *items;
    int arity;

    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return false;

        svm_node node;

        if(!enif_get_int(env, items[0], &node.index))
            return false;

        if(!get_double(env, items[1], &node.value))
            return false;

        feature_vector.push_back(node);
    }

    // Sort the feature vector based on index (required by libsvm)
    std::sort(feature_vector.begin(), feature_vector.end(), [](const svm_node& a, const svm_node& b) {
        return a.index < b.index;
    });

    // push back end node as required by libsvm
    feature_vector.push_back({-1, 0});
    return true;
}

}

void nif_svm_model_free(ErlNifEnv* env, void* obj)
{
    UNUSED(env);

    enif_svm_model* data = static_cast<enif_svm_model*>(obj);

    if(data->model_ != nullptr)
    {
        svm_free_model_content(data->model_);
        data->model_ = nullptr;
    }

    if(data->prob_ != nullptr)
    {
        // release memory for svm_problem

        for (int i = 0; i < data->prob_->l; ++i)
            delete[] data->prob_->x[i];

        delete[] data->prob_->x;
        delete data->prob_;
        data->prob_ = nullptr;
    }
}

ERL_NIF_TERM nif_svm_model_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    ERL_NIF_TERM list = argv[0];

    if(!enif_is_list(env, list))
        return make_badarg(env);

    unsigned int estimated_length;

    if(!enif_get_uint(env, argv[1], &estimated_length))
        return make_badarg(env);

    bool has_gamma_param = false;

    svm_parameter param;
    init_params(&param);

    ERL_NIF_TERM parse_params_result = parse_params(env, argv[2], &param, &has_gamma_param);

    if(parse_params_result != ATOMS.atomOk)
        return parse_params_result;

    // step 1: parse training data

    std::vector<double> labels;
    std::vector<FeatureVector> features;

    labels.reserve(estimated_length);
    features.reserve(estimated_length);

    ERL_NIF_TERM head;
    const ERL_NIF_TERM *items;
    int arity;

    size_t max_feature_size = 0;

    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return make_badarg(env);

        FeatureVector feature;
        double label;

        if(!get_double(env, items[0], &label))
            return make_error(env, "invalid label type");

        if(!parse_feature_vector(env, items[1], feature))
            return make_error(env, "invalid feature vector format");

        if(max_feature_size < feature.size())
            max_feature_size = feature.size();

        labels.push_back(label);
        features.push_back(feature);
    }

    if(!has_gamma_param && max_feature_size > 2)
        param.gamma = 1.0f/static_cast<double>(max_feature_size-1);

    // step 2: prepare the problem data for SVM

    esvm_data* data = static_cast<esvm_data*>(enif_priv_data(env));

    enif_svm_model* enif_obj = static_cast<enif_svm_model*>(enif_alloc_resource(data->resEsvmModel, sizeof(enif_svm_model)));

    if(enif_obj == NULL)
        return make_error(env, esvm::kFailedToAllocResourceMsg);

    svm_problem* prob = new svm_problem;
    prob->l = static_cast<int>(features.size());
    prob->y = new double[prob->l];
    prob->x = new svm_node*[prob->l];

    for (int i = 0; i < prob->l; ++i)
    {
        prob->y[i] = labels[i];
        const FeatureVector& feature = features[i];
        prob->x[i] = new svm_node[feature.size()];

        for (size_t j = 0; j < feature.size(); ++j)
            prob->x[i][j] = feature[j];
    }

    const char* error = svm_check_parameter(prob, &param);

    if(error != nullptr)
    {
        enif_obj->model_ = nullptr;
        enif_obj->prob_ = prob;
        nif_svm_model_free(env, enif_obj);
        enif_release_resource(enif_obj);
        return make_error(env, error);
    }

    // step 3: train svm classifier

    enif_obj->model_ = svm_train(prob, &param);
    enif_obj->prob_ = prob;

    ERL_NIF_TERM term = enif_make_resource(env, enif_obj);
    enif_release_resource(enif_obj);
    return make_ok_result(env, term);
}

ERL_NIF_TERM nif_svm_model_load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string filename;

    if(!get_string(env, argv[0], &filename))
        return make_badarg(env);

    svm_model* model = svm_load_model(filename.c_str());

    if(model == nullptr)
        return make_error(env, "failed to load the model");

    esvm_data* data = static_cast<esvm_data*>(enif_priv_data(env));
    enif_svm_model* enif_obj = static_cast<enif_svm_model*>(enif_alloc_resource(data->resEsvmModel, sizeof(enif_svm_model)));

    if(enif_obj == NULL)
    {
        svm_free_model_content(model);
        return make_error(env, esvm::kFailedToAllocResourceMsg);
    }

    enif_obj->model_ = model;
    enif_obj->prob_ = nullptr;

    ERL_NIF_TERM term = enif_make_resource(env, enif_obj);
    enif_release_resource(enif_obj);
    return make_ok_result(env, term);
}

ERL_NIF_TERM nif_svm_model_save(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_svm_model* enif_obj = NULL;
    esvm_data* data = static_cast<esvm_data*>(enif_priv_data(env));

    if(!enif_get_resource(env, argv[0], data->resEsvmModel, reinterpret_cast<void**>(&enif_obj)))
        return make_badarg(env);

    std::string filename;

    if(!get_string(env, argv[1], &filename))
        return make_badarg(env);

    if(svm_save_model(filename.c_str(), enif_obj->model_) == -1)
        return ATOMS.atomFalse;

    return ATOMS.atomTrue;
}

ERL_NIF_TERM nif_svm_model_predict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_svm_model* enif_obj = NULL;
    esvm_data* data = static_cast<esvm_data*>(enif_priv_data(env));

    if(!enif_get_resource(env, argv[0], data->resEsvmModel, reinterpret_cast<void**>(&enif_obj)))
        return make_badarg(env);

    FeatureVector feature;

    if(!parse_feature_vector(env, argv[1], feature))
        return make_badarg(env);

    return make_ok_result(env, enif_make_double(env, svm_predict(enif_obj->model_, feature.data())));
}

}
