#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

;; protocol.lisp
(docs:define-docs
  (cl:function find-definitions
    "Find applicable definitions for the given designator.

Depending on the type of the designator, the following happens:
  PACKAGE --- FIND-DEFINITIONS is called for each symbol in the
              package, regardless of whether the symbol was
              inherited, or internal.
  STRING  --- FIND-DEFINITIONS is called on the package returned
              by FIND-PACKAGE on the designator. If no package
              is found, an error is signalled.
  T       --- All definitions found by the definition resolvers
              for the designator are returned as a single list.

This function should not error except for the above mentioned
case when a string is passed that does not name a package.

See DEFINITION
See APROPOS-DEFINITIONS
See DEFINITION-RESOLVER")

  (cl:type definition
    "Base class for all definitions.

Any definition type MUST subclass this class.

See DESIGNATOR
See OBJECT
See SYMBOL
See NAME
See PACKAGE
See TYPE
See VISIBILITY
See DOCUMENTATION
See SOURCE-LOCATION")

  (cl:function designator
    "Returns the designator that names this definition.

Note that a single designator may name many definitions.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:function object
    "Returns the object that this definition defines.

The secondary value of this function is :UNKNOWN if no known
method of retrieving the object is available. In that case,
the primary value is always NIL.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:function symbol
    "Returns the symbol naming this definition.

This is different from the designator in the sense that the
designator may be a compound expression that can be mapped to
a single symbol. For instance, the (SETF FOO) designator would
be reduced to the symbol FOO.

This function may return NIL if it is not applicable to reduce
the designator to a symbol.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:function name
    "Returns the name of this definition as a string.

For certain definitions this may be the same as the DESIGNATOR,
or merely a simple mapping of the designator to a string.

This function may return NIL if it is not applicable to reduce
the designator to a string name.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:function package
    "Returns the package this definition is tied to.

Typically this can be derived from the SYMBOL of the definition.
This function is primarily provided for completeness and ease
of use.

This function may return NIL if it is not applicable for the
definition to be tied to a particular package.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:function type
    "Returns the type of the object the definition defines.

For cases where the definition can be instantiated, this is
equivalent to calling TYPE-OF on an instance of such a
definition.

This function may NOT return NIL, and must always return some
symbol that can be used to identify the type of the definition.

Note that this symbol is not necessarily unique to the type of
the definition, meaning multiple definitions may share the same
name.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:function visibility
    "Returns the visibility of the definition from its package.

Returns one of the symbol status designators:
  :INTERNAL :EXTERNAL :INHERITED NIL

A call to this function for any subtype of DEFINITION must
not error.

See CL:FIND-SYMBOL
See DEFINITION")

  (cl:function documentation
    "Returns the documentation string associated with this definition.

This may be NIL if no documentation is defined or known.

The secondary value of this function is :UNKNOWN if no known
method of retrieving the docstring is available. In that case,
the primary value is always NIL.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:function source-location
    "Returns the source location information of this definition.

A successful return will have the form of a plist with the
following keys and values:

  :FILE    --- A pathname to the file in which the definition
               was made. This pathname may be logical.
  :FORM    --- The number of the toplevel form in which this
               definition was made. This means that in order
               to read the definition, this number of calls to
               READ must be made. This value may be NIL.
  :OFFSET  --- The offset in characters at which this
               definition was made. If :FORM is given, this
               offset is within the form that is indicated by
               the :FORM value. Otherwise, this offset is in
               terms of FILE-POSITION from the beginning of
               the file.

This may be NIL if no source location is available or known.

The secondary value of this function is :UNKNOWN if no known
method of retrieving the source location is available. In
that case, the primary value is always NIL.

A call to this function for any subtype of DEFINITION must
not error.

See DEFINITION")

  (cl:type callable
    "Superclass for all definitions that represent objects that may be called.

See DEFINITION
See ARGUMENTS")

  (cl:function arguments
    "Returns the lambda-list of the callable.

The secondary value of this function is :UNKNOWN if no known
method of retrieving the lambda-list is available. In that
case, the primary value is always NIL.

A call to this function for any subtype of CALLABLE must
not error.

See CALLABLE")

  (cl:type global-definition
    "Superclass for global definitions reachable by a simple designator.

This class provides standard implementations for
OBJECT, SYMBOL, NAME, PACKAGE, VISIBILITY, DOCUMENTATION, and
SOURCE-LOCATION. This means that all that's necessary to reach
compliance for a new subtype of GLOBAL-DEFINITION is the
implementation of an appropriate TYPE method.

However, specific implementations of the other functions,
particularly OBJECT, and DOCUMENTATION are usually needed in
order to make all the information available.

See DEFINITION
See DESIGNATOR")

  (cl:variable *definition-resolvers*
    "This variable contains the map from names to definition resolver functions.

See DEFINITION-RESOLVER
See REMOVE-DEFINITION-RESOLVER")

  (cl:function definition-resolver
    "Accessor to the definition resolver of the given name.

A definition resolver is a function that accepts a single
argument, a designator, and returns a list of DEFINITION
instances that are named by this designator.

See DEFINITION
See REMOVE-DEFINITION-RESOLVER
See DEFINE-DEFINITION-RESOLVER
See DEFINE-SIMPLE-DEFINITION-RESOLVER")

  (cl:function remove-definition-resolver
    "Removes the definition resolver of the given name.

See DEFINITION-RESOLVER")

  (cl:function define-definition-resolver
    "Define a new definition resolver.

This is mostly provided as a shorthand.

See DEFINITION-RESOLVER")

  (cl:function apropos-definitions
    "Fuzzily search for definitions matching the given string.

The search is fuzzy in the following way:
If a symbol's name contains the search string regardless of case,
then the symbol is considered to match, and all definitions
designated by the symbol are included in the
result.

See FIND-DEFINITIONS")

  (cl:function define-simple-definition-resolver
    "A simpler form to define a definition resolver.

This comes in two forms, short-form and long-form:

SHORT-FORM
  In this form, only LOOKUP-FUNCTION is provided, and must be a
  symbol that names the test-function.

LONG-FORM
  In this form, LOOKUP-FUNCTION is a lambda-list, and BODY is
  the forms that make up the test-function.

If the test-function returns successfully and returns non-NIL,
then the definition resolver returns a list with a single
instance of the given CLASS, providing the designator as the
initarg for :DESIGNATOR.

The name of the definition resolver is set to CLASS.

See DFINE-DEFINITION-RESOLVER")

  (cl:function define-simple-object-lookup
    "A simpler form to define the OBJECT method.

This comes in two forms, short-form and long-form:

SHORT-FORM
  In this form, only LOOKUP-FUNCTION is provided, and must be
  a symbol that names the lookup-function.

LONG-FORM
  In this form, LOOKUP-FUNCTION is a lambda-list, and BODY is
  the forms that make up the lookup-function.

The lookup-function must accept a single argument, the
definition instance, and must either return the object that the
definition defines, or two values: NIL and :UNKNOWN.

See OBJECT")

  (cl:function define-simple-documentation-lookup
    "A simpler form to define the documentation lookup.

This simply delegates the lookup to CL:DOCUMENTATION, using the
provided arguments. If the DOCUMENTATION-TYPE is T, then 
CL:DOCUMENTATION is called with the OBJECT of the definition.
Otherwise, CL:DOCUMENTATION is called with the DESIGNATOR of the
definition and the provided DOCUMENTATION-TYPE.

See DOCUMENTATION
See CL:DOCUMENTATION
See OBJECT
See DESIGNATOR")

  (cl:function define-simple-type-map
    "A simpler form to define the type lookup.

This simply defines a method on TYPE for the given class,
returning the TYPE symbol.

See TYPE"))

;; base-types.lisp
(docs:define-docs
  (cl:type type
    "Superclass that represents type definitions.

See GLOBAL-DEFINITION")

  (cl:type variable
    "Superclass that represents variable definitions.

See GLOBAL-DEFINITION")

  (cl:type package
    "Package definition representation.

See GLOBAL-DEFINITION")

  (cl:type function
    "Function definition representation.

Note that this does not include macros, compiler-macros, setf-
expanders, or methods.

See GLOBAL-DEFINITION")

  (cl:type macro
    "Macro definition representation.

Note that this does not include compiler-macros, setf-expanders,
or symbol-macros.

See GLOBAL-DEFINITION")

  (cl:type compiler-macro
    "Compiler-macro definition representation.

See GLOBAL-DEFINITION")

  (cl:type setf-expander
    "Setf-expander definition representation.

Note that this is not discoverable portably.
Note that this does not include functions with a (SETF ..)
designator.

See GLOBAL-DEFINITION")

  (cl:type generic-function
    "Generic-function definition representation.

See GLOBAL-DEFINITION")

  (cl:type method
    "Method definition representation.

Note that this is not discoverable portably.

See QUALIFIERS
See GLOBAL-DEFINITION")

  (cl:type method-combination
    "Method-combination definition representation.

Note that this is not discoverable portably.

See GLOBAL-DEFINITION")

  (cl:type class
    "Class definition representation.

Note that this does not include conditions, or structures.

See GLOBAL-DEFINITION")

  (cl:type condition
    "Condition definition representation.

See GLOBAL-DEFINITION")

  (cl:type structure
    "Structure definition representation.

Note that this may not include structures that are defined
as :VECTOR types.

See GLOBAL-DEFINITION")

  (cl:type type-definition
    "Type definition representation.

Note that this is not discoverable portably.
Note that this is only for TYPEDEF-defined types.

See GLOBAL-DEFINITION")

  (cl:type special-variable
    "Special variable definition representation.

Note that this is not discoverable portably.

See GLOBAL-DEFINITION")

  (cl:type constant
    "Constant definition representation.

See GLOBAL-DEFINITION")

  (cl:type symbol-macro
    "Symbol-macro definition representation.

Note that this is not discoverable portably.

See GLOBAL-DEFINITION")

  (cl:function qualifiers
    "Returns the list of qualifiers used for the method.

A call to this function for any subtype of METHOD must
not error.

See METHOD"))

;; sbcl.lisp
(docs:define-docs
  (cl:function define-definition-introspect-type
    "Defines a DEFINITION-SOURCE method that uses FIND-DEFINITION-SOURCES-BY-NAME.

The supplied type-name is passed on to FIND-DEFINITION-SOURCES-BY-NAME.

See DEFINITION-SOURCE
See SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME")

  (cl:function transform-definition-source
    "Translates a DEFINITION-SOURCE instance into the expected format.

See SB-INTROSPECT:DEFINITION-SOURCE")

  (cl:type alien-type
    "Alien-type definition representation.

See GLOBAL-DEFINITION")

  (cl:type optimizer
    "Optimizer definition representation.

See GLOBAL-DEFINITION")

  (cl:type source-transform
    "Source-transform definition representation.

See GLOBAL-DEFINITION")

  (cl:type transform
    "Transform definition representation.

See GLOBAL-DEFINITION")

  (cl:type vop
    "VOP definition representation.

See GLOBAL-DEFINITION")

  (cl:type ir1-convert
    "IR1-Convert definition representation.

See GLOBAL-DEFINITION")

  (cl:type declaration
    "Declaration definition representation.

See GLOBAL-DEFINITION"))
