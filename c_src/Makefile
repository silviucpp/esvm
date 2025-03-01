
PROJECT_NIF_NAME=esvm_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
	LDFLAGS +=  -Wl,-U,_enif_alloc \
				-Wl,-U,_enif_free \
				-Wl,-U,_enif_alloc_resource \
				-Wl,-U,_enif_release_resource \
				-Wl,-U,_enif_get_double \
				-Wl,-U,_enif_get_int \
				-Wl,-U,_enif_get_resource \
				-Wl,-U,_enif_get_uint \
				-Wl,-U,_enif_get_list_cell \
				-Wl,-U,_enif_get_tuple \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_is_list \
				-Wl,-U,_enif_make_double \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_resource \
				-Wl,-U,_enif_make_tuple \
				-Wl,-U,_enif_priv_data \
				-Wl,-U,_enif_open_resource_type \
				-Wl,-U,_enif_inspect_binary
endif

CXXFLAGS += -DNDEBUG -O3 -fPIC -g -Wextra -Wno-deprecated-declarations -Wno-missing-field-initializers -fno-exceptions -fno-rtti -std=c++11
LDFLAGS  += -lstdc++
