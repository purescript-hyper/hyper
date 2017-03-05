*****
Hyper
*****

.. rubric:: Type-safe, statically checked composition of HTTP servers

Hyper is an experimental middleware
architecture for HTTP servers written in PureScript. Its main focus is
correctness and type-safety, using type-level information to enforce correct
composition and abstraction for web servers. The Hyper project is also a
breeding ground for higher-level web server constructs, which tend to fall
under the "framework" category.

.. note::

   Until recently, most work on extensions and higher-level constructs have
   started their life in the main repository, but have then been separated
   into their own packages. More things might be separated in the near future,
   so prepare for unstability. Hyper itself should be considered experimental,
   for now.

This documentation is divided into sections, each being suited for different
types of information you might be looking for.

* :doc:`introduction/index` describes the project itself; its motivations,
  goals, and relevant information for contributors to the project.
* :doc:`core-api/index` is an introduction and reference of the Core API of
  Hyper, on which the higher-level features build.
* :doc:`topic-guides/index` explain how to solve specifics tasks of writing web
  servers using features of Hyper.
* :doc:`tutorials/index` are step-by-step guides on how to build complete
  web applications.
* :doc:`extensions/index` provides an overview of extensions to Hyper.

.. toctree::
  :hidden:

  introduction/index
  core-api/index
  topic-guides/index
  tutorials/index
  extensions/index

