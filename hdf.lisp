(defpackage :hdf5-ffi
  (:use :cl :sb-alien :sb-c-call))

(in-package :hdf5-ffi)

(load-shared-object "libhdf5.so")

(define-alien-type hid_t int)
(define-alien-type order_t int) ; it's an enum
(define-alien-type herr_t int)
(define-alien-type hsize_t (unsigned 64))

(define-alien-routine ("H5Screate_simple" %s-create-simple)
    hid_t
  (rank int)
  (dims (* hsize_t))
  (maxdims (* hsize_t)))

(defun s-create-simple (dimensions)
  (let* ((n (length dimensions))
	 (dims (make-array n :element-type 'fixnum
			   :initial-contents dimensions)))
    (sb-sys:with-pinned-objects (dims)
     (%s-create-simple n
		     (sap-alien (sb-sys:vector-sap dims)
				(* hsize_t)) 
		     (sap-alien (sb-sys:int-sap 0)
				(* hsize_t))))))

(define-alien-routine ("H5open" %open)
    herr_t)

(define-alien-routine ("H5Tcopy" %t-copy)
    hid_t
  (type-id hid_t))

(define-alien-variable ("H5T_NATIVE_FLOAT_g" +native-float+)
    hid_t)

(defconstant +order-le+ 0)

(define-alien-routine ("H5Tset_order" %t-set-order)
    hid_t
  (type-id hid_t)
  (order order_t))

(progn
  (%open)
  (let* ((dataspace (create-simple '(4 5)))
	 (datatype (%t-copy +native-float+))
	 (status (%t-set-order datatype +order-le+)))
    status))