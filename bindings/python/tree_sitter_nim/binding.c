// SPDX-FileCopyrightText: 2024 tree-sitter contributors
//
// SPDX-License-Identifier: MIT

#include <Python.h>

typedef struct TSLanguage TSLanguage;

TSLanguage *tree_sitter_nim(void);

static PyObject* _binding_language(PyObject *self, PyObject *args) {
    return PyCapsule_New(tree_sitter_nim(), "tree_sitter.Language", NULL);
}

static struct PyModuleDef_Slot slots[] = {
#ifdef Py_GIL_DISABLED
    {Py_mod_gil, Py_MOD_GIL_NOT_USED},
#endif
    {0, NULL}
};

static PyMethodDef methods[] = {
    {"language", _binding_language, METH_NOARGS,
     "Get the tree-sitter language for this grammar."},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef module = {
    .m_base = PyModuleDef_HEAD_INIT,
    .m_name = "_binding",
    .m_doc = NULL,
    .m_size = 0,
    .m_methods = methods,
    .m_slots = slots,
};

PyMODINIT_FUNC PyInit__binding(void) {
    return PyModuleDef_Init(&module);
}
