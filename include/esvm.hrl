
-type label() :: non_neg_integer().
-type feature() :: {NodeIndex::non_neg_integer(), NodeValue::number()}.
-type features() :: [feature()].
-type training_params() :: [{binary(), any()}].
-type model() :: reference().

-define(SVM_TYPE_C_SVC, 0).
-define(SVM_TYPE_NU_SVC, 1).
-define(SVM_TYPE_ONE_CLASS, 2).
-define(SVM_TYPE_EPSILON_SVR, 3).
-define(SVM_TYPE_NU_SVR, 4).

-define(KERNEL_TYPE_LINEAR, 0).
-define(KERNEL_TYPE_POLY, 1).
-define(KERNEL_TYPE_RBF, 2).
-define(KERNEL_TYPE_SIGMOID, 3).
-define(KERNEL_TYPE_PRECOMPUTED, 4).
