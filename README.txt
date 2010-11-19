The line breaking program allows the decomposition of a sequence of
objects into a sequence of lines. A typical use would be breaking
a string of text into a set of lines that fit into a particular width.

Line breaking is controlled by an strategy object. It determines the
width of objects, whether objects are blank (e.g. a space), whether
objects trigger new lines (e.g. a newline), whether they are
candidates for line-breaking backtracking (e.g. a dash), etc.

Input and output management are handled by a streamlike object that
supports marking positions in both the input and output streams and
reverting to the marks when needed.

