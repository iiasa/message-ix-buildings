STURM
*****

STURM is one model in the :doc:`MESSAGEix-Buildings <index>` framework.
It is primarily written in R.

Usage and development
=====================

One can use STURM via :mod:`message_ix_buildings` in any of the following ways:

1. Install :mod:`message_ix_buildings`, for example::

     pip install message-ix-buildings

   Locate the installation directory::

     pip show message-ix-buildings

2. Clone the repository from GitHub (https://github.com/iiasa/message-ix-buildings/), for example::

     git clone git@github.com:iiasa/message-ix-buildings.git

   This creates a directory named :file:`message-ix-buildings`.

In either case, all the STURM files are in a further subdirectory named :file:`./message_ix_buildings/sturm/`.
They are organized into 3 directories:

- :file:`./data/` —input data.
- :file:`./model/` —R code.
- :file:`./output/` —example outputs.
