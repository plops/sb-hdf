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

(define-alien-variable ("H5T_NATIVE_FLOAT_g" +t-native-float+)
    hid_t)

(defconstant +order-le+ 0)
(defconstant +p-default+ 0)

(define-alien-routine ("H5Tset_order" %t-set-order)
    hid_t
  (type-id hid_t)
  (order order_t))

;; H5_DLL hid_t H5Dcreate2(hid_t loc_id, const char *name, hid_t type_id,
;;     hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id);
;; H5_DLL hid_t H5Dcreate1(hid_t file_id, const char *name, hid_t type_id,
;;     hid_t space_id, hid_t dcpl_id);

(define-alien-routine ("H5Dcreate2" %d-create2)
    hid_t
  (loc-id hid_t)
  (name (c-string))
  (type-id hid_t)
  (space-id hid_t)
  (lcpl-id hid_t)
  (dcpl-id hid_t)
  (dapl-id hid_t))

(define-alien-routine ("H5Dcreate1" %d-create1)
    hid_t
  (loc-id hid_t)
  (name (c-string))
  (type-id hid_t)
  (space-id hid_t)
  (lcpl-id hid_t))

;; H5_DLL hid_t  H5Fcreate(const char *filename, unsigned flags,
;; 		  	  hid_t create_plist, hid_t access_plist);
(define-alien-routine ("H5Fcreate" %f-create)
    hid_t
  (filename (c-string))
  (flags unsigned-int)
  (create-plist hid_t)
  (access-plist hid_t))

(defconstant +f-acc-trunc+ 2)

;; H5_DLL hid_t  H5Fopen(const char *filename, unsigned flags,
;; 		        hid_t access_plist);
(define-alien-routine ("H5Fopen" %f-open)
    hid_t
  (filename (c-string))
  (flags unsigned-int)
  (access-plist hid_t))

;; H5_DLL herr_t H5Dclose(hid_t dset_id);
(define-alien-routine ("H5Dclose" %d-close)
    herr_t
  (dset-id hid_t))

;; H5_DLL herr_t H5Fclose(hid_t file_id);
(define-alien-routine ("H5Fclose" %f-close)
    herr_t
  (file-id hid_t))

;; H5_DLL herr_t H5Sclose(hid_t space_id);
(define-alien-routine ("H5Sclose" %s-close)
    herr_t
  (space-id hid_t))

;; H5_DLL herr_t H5Tclose(hid_t type_id);
(define-alien-routine ("H5Tclose" %t-close)
    herr_t
  (type-id hid_t))

;; H5_DLL herr_t H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
;; 			 hid_t file_space_id, hid_t plist_id, const void *buf);
(define-alien-routine ("H5Dwrite" %d-write)
    herr_t
  (dset-id hid_t)
  (mem-type-id hid_t)
  (mem-space-id hid_t)
  (file-space-id hid_t)
  (plist-id hid_t)
  (buf (* unsigned-char)))

(defconstant +s-all+ 0)

;; H5_DLL herr_t H5get_libversion(unsigned *majnum, unsigned *minnum,
;; 				unsigned *relnum);
;; H5_DLL herr_t H5check_version(unsigned majnum, unsigned minnum,
;; 			       unsigned relnum);

(define-alien-routine ("H5check_version" %check-version)
    herr_t
  (majnum unsigned-int)
  (minnum unsigned-int)
  (relnum unsigned-int))

(defun check ()
 (let ((data (make-array (list 3 3) :element-type 'single-float)))
   (%open)
   ;; (%check-version 1 8 4)

   (let* ((file (%f-create "test.h5" +f-acc-trunc+ +p-default+ +p-default+))
	  (dataspace (s-create-simple (reverse (array-dimensions data))))
	  (datatype (%t-copy +t-native-float+))
	  (status (%t-set-order datatype +order-le+))
	  (dataset (%d-create2 file "FloatArray" datatype dataspace
			       +p-default+ +p-default+ +p-default+))
	  (status2 (sb-sys:with-pinned-objects (data)
		     (let* ((data1 (sb-ext:array-storage-vector data))
			    (data1-sap (sb-sys:vector-sap data1))
			    (data1-alien (sap-alien data1-sap
						    (* unsigned-char))))
		       (%d-write dataset +t-native-float+ +s-all+
				 +s-all+ +p-default+ data1-alien)))))
     (%s-close dataspace)
     (%t-close datatype)
     (%d-close dataset)
     (list (%f-close file) status status2))))

#+nil
(time
 (check))