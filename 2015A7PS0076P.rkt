#lang racket

(provide step1)
(provide step2)
(provide step3)
(provide step4)
(provide step5)
(provide step6)
(provide step7)
(provide step8)
(provide step9)
(provide step10)

(define (parse_string list_str)
  (if (null? list_str)
      '( )
      (append (list (string->number (car list_str))) (parse_string (cdr list_str)))
      )
  )

(define input (file->lines (vector-ref (current-command-line-arguments) 0)))

(define arguments (parse_string (string-split (car input))))
(define N (car arguments))
(define D (car (cdr arguments)))
(define K (car (cdr (cdr arguments))))
(define eps (car (cdr (cdr (cdr arguments)))))
(define Minpts (car (cdr (cdr (cdr (cdr arguments))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP1


(define (form_input_matrix input index)
  (if (null? input)
      '( )
      (append (list (list index (parse_string (string-split (car input))))) (form_input_matrix (cdr input) (+ index 1)))
      )
  )


(define matrix_input (form_input_matrix (cdr input) 1))
(define step1 matrix_input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP2


(define (distance_points point1 point2)
	(if (null? point1)
		0
		(+ (expt (- (car point1) (car point2)) 2) (distance_points (cdr point1) (cdr point2)))
	)
)

(define (calculate_distance_row row_number index row input_matrix)
  (if (null? input_matrix)
      '( )
      (if (equal? row_number index)
          (append (list (list index +inf.0)) (calculate_distance_row row_number (+ index 1) row (cdr input_matrix)))
          (append (list (list index (sqrt (distance_points row (car (cdr (car input_matrix))))))) (calculate_distance_row row_number (+ index 1) row (cdr input_matrix)))          
          )      
      )
  )

(define (calculate_distance_matrix input index)
  (if (null? input)
      '( )
      (append (list (calculate_distance_row index 1 (car (cdr (car input))) step1)) (calculate_distance_matrix (cdr input) (+ index 1)))
      )
  )


(define distance_matrix (calculate_distance_matrix step1 1))

(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

(define distance_matrix_modified (modify_precision distance_matrix))

(define step2 distance_matrix_modified)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP3

(define (sort_on_distance list_of_distances_from_point)
  (sort list_of_distances_from_point
        (lambda (tuple1 tuple2)
          (or (< (car (cdr tuple1)) (car (cdr tuple2)))
              (and (= (car (cdr tuple1)) (car (cdr tuple2)))
                   (< (car tuple1) (car tuple2))))))
  )

(define (KNN index K distance_row)
  (if (equal? index (+ K 1))
      '( )
      (append (list (car (car distance_row))) (KNN (+ index 1) K (cdr distance_row)))      
      )  
  )

(define (calculate_KNN K distance_matrix)
  (if (null? distance_matrix)
      '( )
      (append (list (sort (KNN 1 K (sort_on_distance(car distance_matrix))) <)) (calculate_KNN K (cdr distance_matrix)))      
      )
 )

(define KNearest_Neighbours (calculate_KNN K step2))
(define step3 KNearest_Neighbours)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP4

(define (intersect list1 list2)
  (if (null? list1)
      '( )
      (if (member (car list1) list2)
          (append (list (car list1)) (intersect (cdr list1) list2))
          (intersect (cdr list1) list2)
          )
      )
  )

(define (sort_by_weight_and_index list)
  (sort list
        (lambda (tuple1 tuple2)
          (or (> (car (cdr tuple1)) (car (cdr tuple2)))
              (and (= (car (cdr tuple1)) (car (cdr tuple2)))
                   (< (car tuple1) (car tuple2))))))
  )

(define (make_edges_for_point point_no point point_complete)
   (if (null? point)
       '( )
       (if (and (member point_no (list-ref step3 (- (car point) 1))) (not (equal? point_no (car point))))
           (append (list (list (car point) (length (intersect point_complete (list-ref step3 (- (car point) 1)))))) (make_edges_for_point point_no (cdr point) point_complete))
           (make_edges_for_point point_no (cdr point) point_complete)          
           )       
       )
  )

(define (make_graph index nearest_neighbours_matrix)
  (if (null? nearest_neighbours_matrix)
      '( )
      (append (list (sort_by_weight_and_index (make_edges_for_point index (car nearest_neighbours_matrix) (car nearest_neighbours_matrix)))) (make_graph (+ index 1) (cdr nearest_neighbours_matrix)))
      )
  )

(define graph (make_graph 1 step3))
(define step4 graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP5

(define (filter_edges_for_point edges_from_point)
  (if (null? edges_from_point)
      '( )
      (if (>= (car (cdr (car edges_from_point))) eps)
          (append (list (car (car edges_from_point))) (filter_edges_for_point (cdr edges_from_point)))
          (filter_edges_for_point (cdr edges_from_point))
          )
      )
  )


(define (calculate_densities edge_graph)
  (if (null? edge_graph)
      '( )
      (append (list (length (filter_edges_for_point (car edge_graph)))) (calculate_densities (cdr edge_graph)))
      )
  )


(define density_list (calculate_densities step4))
(define step5 density_list)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP6

(define (get_core_points densities index)
  (if (null? densities)
      '( )
      (if (>= (car densities) Minpts)
          (append (list index) (get_core_points (cdr densities) (+ index 1)))
          (get_core_points (cdr densities) (+ index 1))
          )
      )
  )

(define core_pts (get_core_points step5 1))
(define step6 core_pts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP7

(define (check_member point_no clusters)
  (if(null? clusters)
     #f
     (if (member point_no (car (cdr (car clusters))))
         #t
         (check_member point_no (cdr clusters))
         )
     )
  )

(define (add_neighbours_to_queue edges_from_point clusters queue edge_graph)
  (if (null? edges_from_point)
      queue
      (if (and (member (car (car edges_from_point)) step6) (not (member (car (car edges_from_point)) queue)) (not (check_member (car (car edges_from_point)) clusters)) (>= (car (cdr (car edges_from_point))) eps))
          (add_neighbours_to_queue (cdr edges_from_point) clusters (append queue (list(car (car edges_from_point)))) edge_graph)
          (add_neighbours_to_queue (cdr edges_from_point) clusters queue edge_graph)
          )
      )
  )

(define (bfs edge_graph clusters encountered queue index)
  (if (equal? index (+ (length queue) 1))
      encountered
      (if (member (list-ref queue (- index 1)) encountered)
          (bfs edge_graph clusters encountered queue (+ index 1))
          (bfs edge_graph clusters (append encountered (list (list-ref queue (- index 1)))) (add_neighbours_to_queue (list-ref edge_graph (- (list-ref queue (- index 1)) 1)) clusters queue edge_graph ) (+ index 1))
          )      
      )
  )

(define (form_clusters clusters core_pts index edge_graph)
  (if (null? core_pts)
      clusters
      (if (check_member (car core_pts) clusters)
          (form_clusters clusters (cdr core_pts) index edge_graph)
          (form_clusters (append clusters (list (list index (sort (bfs edge_graph clusters '( ) (list (car core_pts)) 1) <)))) (cdr core_pts) (+ 1 index) edge_graph)
          )
      )
  )

(define clusters (form_clusters '( ) step6 1 step4))
(define step7 clusters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP8

(define (second_condition_for_noise edges_from_point)
  (if (null? edges_from_point)
      #t
      (if (>= (car (cdr (car edges_from_point))) eps)
          #f
          (second_condition_for_noise (cdr edges_from_point))
          )
      )
  )

(define (get_noise_points edge_graph core_pts index)
  (if (equal? index (+ N 1))
      '( )
      (if (and (not(member index core_pts)) (second_condition_for_noise (list-ref edge_graph (- index 1))))
          (append (list index) (get_noise_points edge_graph core_pts (+ 1 index)) )
          (get_noise_points edge_graph core_pts (+ 1 index))
          )
      )  
  )

(define noise_pts (get_noise_points step4 step6 1))
(define step8 noise_pts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP9

(define (get_border_points core_pts noise_pts index)
  (if (equal? index (+ N 1))
      '( )
      (if (and (not(member index core_pts)) (not(member index noise_pts)))
          (append (list index) (get_border_points core_pts noise_pts (+ 1 index)) )
          (get_border_points core_pts noise_pts (+ 1 index))
          )
      )  
  )

(define border_pts (get_border_points step6 step8 1))
(define step9 border_pts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STEP10

(define (find_cluster_id clusters core_point_no)
  (if (member core_point_no (car (cdr (car clusters))))
      (car (car clusters))
      (find_cluster_id (cdr clusters) core_point_no)
      )      
  )

(define (append_border_point border_point_no cluster_id clusters)
  (list cluster_id (sort (append (car (cdr (list-ref clusters (- cluster_id 1)))) (list border_point_no)) <))
  )

(define (sim point1 point2)
   (length (intersect (list-ref step3 (- point1 1)) (list-ref step3 (- point2 1))))
  )

(define (find_similarity_list border_point_no core_pts)
  (if (null? core_pts)
      '( )
      (append (list (sim (car core_pts) border_point_no)) (find_similarity_list border_point_no (cdr core_pts)))
      )
  )

(define (find_core_point core_pts similarity_list index max max_index) 
  (if (null? similarity_list)
      (list-ref core_pts (- max_index 1))
      (if (>(car similarity_list) max)
          (find_core_point core_pts (cdr similarity_list) (+ 1 index) (car similarity_list) index)
          (find_core_point core_pts (cdr similarity_list) (+ 1 index) max max_index)
          )
      )
  )

(define (list-with clusters index new_cluster)
  (if (null? clusters)
    clusters
    (cons
      (if (zero? (- index 1))
        new_cluster
        (car clusters)
        )
      (list-with (cdr clusters) (- index 1) new_cluster)
      )
    )
  )

(define (replaceCluster clusters new_cluster)
  (list-with clusters (car new_cluster) new_cluster)
  )

(define (assign_border_points_to_clusters clusters border_points)
  (if (null? border_points)
      clusters
      (assign_border_points_to_clusters (replaceCluster clusters (append_border_point (car border_points) (find_cluster_id clusters (find_core_point step6 (find_similarity_list (car border_points) step6) 1 -1 0)) clusters))  (cdr border_points))
      )
  )

(define final_clusters (assign_border_points_to_clusters step7 step9))
(define step10 final_clusters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
