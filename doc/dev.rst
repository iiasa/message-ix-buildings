Development
***********

Build the package
=================

Ensure the Python package `build <https://pypa-build.readthedocs.io>`__ is installed::

    pip install build

In the project base directory, run::

    python -m build

Build the documentation
=======================

Change into the :file:`./doc/` directory::

    cd doc

Build (for instance) the HTML documentation::

    make html

The terminal output indicates “The HTML pages are in _build/html”.
Open the file :file:`index.html` in this directory.

Write and run tests
===================

The :mod:`message_ix_buildings` package has some tests.

Run these using `pytest <https://docs.pytest.org>`__::

    pytest
