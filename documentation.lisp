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

The package argument designates the package that the global
definitions should be relative to. This is mostly relevant for
the visibility of the definition.

The type argument allows you to constrain the search to
definitions of the given type.

See DEFINITION
See APROPOS-DEFINITIONS
See DEFINITION-RESOLVER")

  (cl:function definition-p
    "Test whether a designator is designating a definition of the given type.

You may pass T for the type to test whether the designator is
designating any definition at all.

This has the same semantics as calling FIND-DEFINITIONS with the
designator, package, and type passed along, and checking that
the returned list of definitions isn't empty.

See FIND-DEFINITIONS")

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
               to read the definition, this number of calls+1
               to READ must be made. This value may be NIL.
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

  (cl:type binding-exists
    "Error signalled when a new binding conflicts with an existing one.

The condition contains the DESIGNATOR and TYPE that the conflicting
binding was intended to be for.

See TYPE
See DESIGNATOR
See BIND")

  (cl:function bind
    "Creates a new definition binding.

Returns the canonical designator for the new binding.

DESIGNATOR must be a designator to which a binding should be
established. The designator may be normalised to some other, canonical
designator.

TYPE must be the name or an instance of a definition type for which a
new binding should be established. If it is a name, it is
automatically coerced to an instance of the definition type. Note that
this parameter is purely for dispatch purposes. Modifying or
inspecting the instance in any way leads to undefined behaviour.

OBJECT must be some object in a predefined format that contains the
definition information to which the new binding will point. What the
structure of this object should be depends on the definition to bind.

If a binding of the given type for the given designator already
exists, or the new binding would conflict with another existing
binding in some way, an error of type BINDING-EXISTS must be
signalled.

If the binding protocol is not implemented, an error of type
NO-APPLICABLE-METHOD is signalled.

If you add custom definition types, you are encouraged to add methods
to this function that allow standardised addition of new definition
bindings.

Once a call to this function returns successfully, DEFINITION-P of the
used designator and type must return T.

See (SETF OBJECT)
See UNBIND")

  (cl:function (setf object)
    "Updates an existing definition binding.

Returns the passed OBJECT.

OBJECT must be some object in a predefined format that contains the
definition information to which the new binding will point. What the
structure of this object should be depends on the definition to bind.

If the updating protocol is not implemented, an error of type
NO-APPLICABLE-METHOD is signalled.

If you add custom definition types, you are encouraged to add methods
to this function that allow standardised updating of definition
bindings.

See BIND
See UNBIND")

  (cl:function unbind
    "Removes the definition binding.

If the given definition is already unbound, this function does
nothing.

If the unbinding protocol is not implemented for the given definition
type, an error of type NO-APPLICABLE-METHOD is signalled.

If you add custom definition types, you are encouraged to add methods
to this function that allow standardised removal of definition
bindings.

Once a call to this function returns successfully, DEFINITION-P of the
designator and type of the definition must return NIL.

See (SETF OBJECT)
See BIND")

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

A definition resolver is a function that accepts two arguments, 
a designator and a package, and returns a list of DEFINITION
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

See DEFINE-DEFINITION-RESOLVER")

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
    "Defines a SOURCE-LOCATION method that uses FIND-DEFINITION-SOURCES-BY-NAME.

The supplied type-name is passed on to FIND-DEFINITION-SOURCES-BY-NAME.

See SOURCE-LOCATION
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
