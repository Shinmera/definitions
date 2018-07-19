## About Definitions
This system implements a general definitions introspection library. It gives you the ability to retrieve definitions or bindings associated with designators such as symbols, packages, and names in general. For instance, it allows you to retrieve all function, type, variable, method, etc. definitions of a symbol.

## How To
In order to list all discoverable definitions in a package, all you need to do is call `find-definitions` like so:

    (find-definitions "CL")

Each `definition` is associated with a `designator` that names it. Most, but not all, definitions also allow you to retrieve the `object` that they define. If the designator is translatable to a symbol, the definition will also give you access to the `symbol`, `name`, `package`, and `visibility` within the package. Beyond that, you can also retrieve the associated `documentation` and, depending on implementation support, the `source-location` of the definition.

Some subclasses of the general `definition`, like `callable` give you access to further information like the `arguments` or the `qualifiers`.

If you would like to fuzzily search for definitions, you can use the `apropos-definitions` function. Do note that this will likely return very long lists of definitions for searches that aren't very long.

## Extending Definitions
Often time libraries and systems will introduce their own bindings and definitions that extend the environment. For instance, you might have a special kind of function definition that lives in its own namespace, or a particular kind of named resource. In this case it can be worthwhile to extend Definitions so that it can discover and handle information related to those definitions as well.

Adding new definition types is a matter of subclassing `definition`, using `define-definition-resolver` to handle the definition discovery, and adding methods to the relevant generic functions to provide the needed information.

Typically your definition will be a `global-definition`, and thus be tied to a simple designator like a symbol that will find the definition in a global namespace. The `global-definition` provides default implementations for `symbol`, `name`, `package`, and `visibility`, removing much of the need for duplicating those methods.

    (defclass page (global-definition) ())

    (define-simple-type-map page my-package:page)
    (define-simple-definition-resolver page my-package:find-page)
    (define-simple-object-lookup page my-package:find-page)
    (define-simple-documentation-lookup page my-package:page)

The above will define a new definition type, `page`, and add the missing methods to it, provided that the documentation is already handled through `cl:documentation` on the `my-package:page` type, and that `my-package:find-page` returns the page object associated with a name. From there on out, `find-definitions` will pick up `page` definitions as well. 

Depending on how your implementation tracks source information, and how well it is supported by this library, `source-information` might work automatically on new definition types once a suitable `object` method is in place. However, in general, it is not guaranteed that source tracking will be provided, let alone for definitions that are not standard.

## Currently Supported Implementations
Some parts of this library such as the discovery of certain definitions, retrieval of some object bindings, and the source location tracking are implementation-specific. As such, in order to be complete, implementation-specific parts need to be written. Currently the following implementations are *fully* supported. Note that even if your implementation is not listed here, this library may still be useful.

* SBCL
* CCL
* Clasp
