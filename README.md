# replay-streams
### _Boutade <thegoofist@protonmail.com>_

`replay-streams` let the programmer rewind an input stream to a previous point
so that the contents of the stream may be read again. This provides input
streams with a form of look-ahead.

When you instantiate a `replay-character-stream` you pass it an character input
stream to use as the underlying character source. You can then read from your
new string like a normal character input stream.

The `rewind` method will rewind the stream only once. If the stream has already
been rewound, `rewind` returns `nil`.

## Example

```

CL-USER> (ql:quickload 'replay-streams)    ;; NOT IN QUICKLISP. Install into quicklisp/local-projects/
To load "replay-streams":
  Load 1 ASDF system:
    replay-streams
; Loading "replay-streams"
[package replay-streams]
(REPLAY-STREAMS)

CL-USER> (use-package :replay-streams)
T

CL-USER> (defvar *source* (make-string-input-stream "Hey there here is a string!"))
*SOURCE*

CL-USER> (defvar *rpstream* (make-instance 'replay-character-stream 
                                           :source *source*))
                                           
CL-USER> (let ((buf (make-string 10)))
           (read-sequence buf *rpstream*)
           buf)
"Hey there "

CL-USER> (let ((buf (make-string 10)))
           (read-sequence buf *rpstream*)
           buf)
"here is a "

CL-USER> (rewind *rpstream*)
#<SB-IMPL::STRING-INPUT-STREAM {10042D0F83}>

CL-USER> (let ((buf (make-string 10)))
           (read-sequence buf *rpstream*)
           buf)
"Hey there "

CL-USER> (let ((buf (make-string 10)))
           (read-sequence buf *rpstream*)
           buf)
"here is a "

CL-USER> (let ((buf (make-string 10)))
           (read-sequence buf *rpstream*)
           buf)
"string!   "

CL-USER> (rewind *rpstream*)       ;; YOU CAN ONLY REWIND ONCE
NIL


```

## Notes

The interface presently only supports character streams but I see no reason that
byte/binary streams couldn't be supported as well. I've added this to the *To Do* 
section below.  

The implementation tries to be as responsible as it can be with respect to
memory and open streams. Specifically, it will free its internal buffers when
they are no longer needed and close streams that are no longer of any use.

Given that the look-ahead depth is arbitrary, you could potentially waste a ton
of memory with irresponsible use of this package. A good rule of thumb is to try
and keep reference to the underying "source" stream around so that, when you no
longer need any replay and when the `(replay-finished rp-stream)` is true.

## To Do

- [ ] Add support for byte oriented streams
- [ ] Make test suite
- [ ] Refactor to use stream concatenation where applicable
