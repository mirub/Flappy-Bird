#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "random.rkt")
(require "abilities.rkt")
(require "constants.rkt")
;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

;---------------------------------------checker_exports------------------------------------------------
; Checker-ul contine un numar de teste, fiecare cu numele sau. In acest fisier veti gasi comentarii
; care incep cu TODO %nume_test, unde trebuie sa modificati sau sa implementati o functie, pentru
; a trece testul %nume_test.
;
;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Recomandam ca in pasare, sa retineti, printre altele, informatii despre y-ul curent
; si viteza pe y
; Pe parcursul temei, in state, salvati coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara atat logica miscarii obiectelor, cat si testarea cerintelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor!
;Inițial state
; Primul pas pe care trebuie să îl facem este să creăm starea inițială a jocului.
; Aceasta va fi salvată în (get-initial-state), și trebuie să incapsuleze toate informațiile
; necesare jocului, și anume: informații despre pasăre, despre pipes și, pentru bonus,
; despre powerups și despre variabilele de mediu.
; Recomandăm ca în pasăre, să rețineți, printre altele, informații despre y-ul curent
; și viteză pe y.
; Pe parcursul temei, în state, salvați coordonatele colțurilor din stânga sus ale obiectelor.
; Aceasta va face mai ușoară atât logică mișcării obiectelor, cât și testarea cerințelor.
; Toate coordonatele oferite în comentarii sau în fișierul variables.rkt se referă la
; colțul din stânga sus ale obiectelor!

;TODO 1
; După ce definiți structurile lui (get-initial-state) și a păsării, introduceți în prima
; pe cea din urmă. Colțul din stânga sus a păsării se va află inițial la:
;    y = bird-inițial-y
; și x = bird-x.
; (get-initial-state) va fi o funcție care va returna starea inițială a jocului.

;TODO 8
; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definiți structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugați primul pipe în starea jocului. Acesta se va află inițial în afară ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Atenție! Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
; Atenție! Recomandăm să păstrați în stare colțul din stânga sus al chenarului lipsa
; dintre cele 2 pipe-uri!

;TODO 16
; Vrem o modalitate de a păstra scorul jocului. După ce definiți structura
; acestuia, adăugați scorul inițial, adică 0, în starea inițială a jocului.
; Atenție get-initial-state trebuie sa fie o funcție
; și trebuie apelată în restul codului.

(define-struct bird-struct (x y speed) #:transparent)
(define initial-bird (bird-struct bird-x bird-initial-y 0))

(define-struct pipe-struct (x-gap y-gap) #:transparent)
(define initial-pipe (pipe-struct scene-width (+ added-number (random random-threshold))))

(define-struct pipe-list-struct (pipes) #:transparent)
(define initial-pipe-list (pipe-list-struct (cons initial-pipe null)))

(define-struct state-struct (bird pipe-list score variables abilities) #:transparent)

(define (get-initial-state)
  (state-struct initial-bird  (pipe-list-struct (cons (pipe-struct scene-width (+ added-number (random random-threshold))) null))
                0 (variable-struct initial-gravity initial-momentum initial-scroll-speed) (ability-types-struct null null)))

;TODO 2
; După aceasta, implementați un getter care extrage din structura voastră
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află această.

(define (get-bird state)
  (state-struct-bird state))

(define (get-bird-y bird)
  (bird-struct-y bird))

;TODO 3
; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adaugă
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.

; Functia returneaza noua coordonata y a pasarii
(define (get-new-y bird) 
  (+ (bird-struct-y bird) (bird-struct-speed bird)))

; Functia returneaza noua viteza a pasarii
(define (get-new-speed bird gravity)
  (+ (bird-struct-speed bird) gravity))

(define (next-state-bird bird gravity)
    (bird-struct (bird-struct-x bird) (get-new-y bird) (get-new-speed bird gravity))) ; NEXT STATE

;TODO 4
; După aceasta, implementati un getter care extrage din structura voastră
; viteza pe y a păsării.
(define (get-bird-v-y bird)
  (bird-struct-speed bird))

;TODO 6
; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definiți funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimba viteza
; pe y a păsării cu -momentum.

(define (compute-new-speed momentum)
  (- 0 momentum))

(define (next-state-bird-onspace bird momentum)
    (bird-struct (bird-struct-x bird) (bird-struct-y bird) (compute-new-speed momentum)))

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.
;TODO 7
; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.

(define (change state pressed-key)
  (match-let ([current-bird (get-bird state)])
    (cond [(key=? pressed-key " ")
           (let* ((new-bird (next-state-bird-onspace current-bird initial-momentum))
                  (current-pipes (state-struct-pipe-list state))
                  (current-score (state-struct-score state)))
             (state-struct new-bird current-pipes current-score (state-struct-variables state) (state-struct-abilities state)))]
          [else state])))

;TODO 9
; După ce ați definit structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementați getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.

(define (get-pipes state)
  (pipe-list-struct-pipes (state-struct-pipe-list state)))

;TODO 10
; Implementați get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.

(define (get-pipe-x pipe)
  (pipe-struct-x-gap pipe))

;TODO 11
; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va scădea din x-ul fiecărui pipe
; scroll-speed-ul dat.

; Functia calculeaza noul x pentru un pipe
(define (get-new-x pipe scroll-speed)
  (- (get-pipe-x pipe) scroll-speed))

(define (move-pipes pipes scroll-speed)
  (map
   (λ (pipe)
       (struct-copy pipe-struct pipe [x-gap (get-new-x pipe scroll-speed)]))
   pipes))

;TODO 12
; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din DREAPTA sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.
;
; Hint: cunoaștem lățimea unui pipe, pipe-width

; Functia sterge recursiv pipes ce nu respecta cerinta
(define (remove-pipe pipes new-pipes)
  (cond
    [(null? pipes) new-pipes]
    [(< (+ (get-pipe-x (car pipes)) pipe-width) 0) (append (remove-pipe (cdr pipes) new-pipes) '())]
    [else (append (cons (car pipes) '()) (remove-pipe (cdr pipes) new-pipes))])
  )

(define (clean-pipes pipes)
  (remove-pipe pipes '()))


;TODO 13
; Vrem să avem un sursa continuă de pipe-uri.
; Implementati funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.

; Functia returneaza cel mai departat pipe de pasare
(define (get-furthest-pipe pipes)
  (car (reverse pipes)))

; Functia calculeaza noua coordonata X a pipe-ului
(define (get-new-pipe-x furthest-pipe)
  (+ (+ pipe-width pipe-gap) (get-pipe-x furthest-pipe)))

(define (add-more-pipes pipes)
  (cond
    [(< (length pipes) no-pipes)
        (append pipes (cons (pipe-struct (get-new-pipe-x (get-furthest-pipe pipes)) (+ added-number (random random-threshold))) '()))]
    [else pipes]))


;TODO 14
; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
; move-pipes, urmat de clean-pipes, urmat de add-more pipes.

(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

;TODO 17
; Creați un getter ce va extrage scorul din starea jocului.
(define (get-score state)
  (state-struct-score state))

;TODO 19
; Vrem să creăm logica coliziunii cu pământul.
; Implementati check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.
;
; Hint: știm înălțimea păsării, bird-height, și y-ul pământului, ground-y.
; Coliziunea ar presupune ca un colț inferior al păsării să aibă y-ul
; mai mare sau egal cu cel al pământului.

; Functia calculeaza coordonata y de jos a pasarii
(define (compute-lower-y-corner bird)
  (+ (bird-struct-y bird) bird-height))

(define (check-ground-collision bird)
     (cond
       [(< (compute-lower-y-corner bird) ground-y) #f]
       [else #t]))

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.

;TODO 20
; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.

;TODO 22
; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.

(define (invalid-state? state)
  (cond
    [(check-ground-collision (get-bird state)) #t]
    [(check-pipe-collisions (get-bird state) (get-pipes state)) #t]
    [else #f]
  ))

;TODO 21
; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementati funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.
;
; Hint: Vă puteți folosi de check-collision-rectangle, care va primi drept parametri
; colțul din stânga sus și cel din dreapta jos ale celor două dreptunghiuri
; pe care vrem să verificăm coliziunea.

; Coltul din stanga sus al pasarii
(define (get-top-left-corner-bird brd)
  (make-posn (bird-struct-x brd) (get-bird-y brd)))

; Coltul din dreapta sus al pasarii
(define (get-top-right-corner-bird brd)
  (make-posn (+ (bird-struct-x brd) bird-width) (get-bird-y brd)))

; Coltul din stanga jos al pasarii
(define (get-bottom-left-corner-bird brd)
  (make-posn (+ (bird-struct-x brd) bird-width) (+ (get-bird-y brd) bird-height)))

; Coltul din dreapta jos al pasarii
(define (get-bottom-right-corner-bird brd)
  (make-posn (+ (bird-struct-x brd) bird-width) (+ (get-bird-y brd) bird-height)))

; Coltul din stanga sus al pipe
(define (get-top-left-corner-pipe pipe)
  (make-posn (pipe-struct-x-gap pipe) 0))

; Coltul din dreapta sus al pipe
(define (get-top-right-corner-pipe pipe)
  (make-posn (+ (pipe-struct-x-gap pipe) pipe-width) (pipe-struct-y-gap pipe)))

; Coltul din stanga jos al pipe
(define (get-bottom-left-corner-pipe pipe)
  (make-posn (pipe-struct-x-gap pipe) (+ (pipe-struct-y-gap pipe) pipe-self-gap)))

; Coltul din dreapta jos al pipe
(define (get-bottom-right-corner-pipe pipe)
  (make-posn (+ (pipe-struct-x-gap pipe) pipe-width) pipe-height))

(define (check-pipe-collisions brd pipes)
  (cond [(null? pipes) #f]       
        [else (cond [(or (check-collision-rectangles (get-top-left-corner-bird brd) (get-top-right-corner-bird brd)
                                              (get-top-left-corner-pipe (car pipes)) (get-top-right-corner-pipe (car pipes)))
                  (check-collision-rectangles (get-bottom-left-corner-bird brd) (get-bottom-right-corner-bird brd)
                                              (get-bottom-left-corner-pipe (car pipes)) (get-bottom-right-corner-pipe (car pipes)))) #t]
             [else (check-pipe-collisions brd (cdr (list pipes)))])]))

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

;Next-state
; Next-state va fi apelat de big-bang la fiecare cadru, pentru a crea efectul de
; animație. Acesta va primi ca parametru o structură de tip stare, și va întoarce
; starea corespunzătoare următorului cadru.

;TODO 5
; Trebuie să integrăm funcția implementată anterior, și anume next-state-bird,
; în next-state.

;TODO 15
; Vrem să implementăm logică legată de mișcarea, ștergerea și adăugarea pipe-urilor
; în next-state. Acesta va apela next-state-pipes pe pipe-urile din starea curentă.

;TODO 18
; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.

; Functia calculeaza noul scor
(define (get-increased-score state)
  (+ (state-struct-score state) 0.1))

; Functia returneaza lista de pipe din noua stare
(define (get-next-pipe-list state)
  (pipe-list-struct (next-state-pipes (get-pipes state) initial-scroll-speed)))

; Functia returneaza pasarea din noua stare
(define (get-next-bird state)
  (next-state-bird (get-bird state) initial-gravity))

(define (next-state state)
  (state-struct (get-next-bird state) (get-next-pipe-list state) (get-increased-score state)
                (next-variables (state-struct-variables state) (state-struct-abilities state))
                (next-abilities (state-struct-abilities state) (get-next-bird state)
                                                                            (variable-struct-scroll-speed (state-struct-variables state)))))

; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.
;TODO 23
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Hint: score-to-image primeste un numar real si intoarce scor-ul sub forma de imagine;
; Scor-ul îl puteți plasa direct la coordonatele date, fără a mai face translatiile menționate mai jos.
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

; Calculeaza coordonata initiala X a pasarii
(define (compute-initial-bird-x brd)
  (+ (bird-struct-x brd) (quotient bird-width 2)))

; Calculeaza coordonata initiala Y a pasarii
(define (compute-initial-bird-y brd)
  (+ (bird-struct-y brd) (quotient bird-height 2)))

; Calculeaza coordonata initiala X a ground
(define (compute-initial-ground-x)
  (+ 0 (quotient scene-width 2)))

; Calculeaza coordonata initiala Y a ground
(define (compute-initial-ground-y)
  (+ ground-y (quotient ground-height 2)))

(define (draw-frame state)
  (let* ([brd (get-bird state)])
    (place-image bird-image (compute-initial-bird-x brd) (compute-initial-bird-y brd)
                 (place-image ground-image (compute-initial-ground-x) (compute-initial-ground-y)
                              (place-image (score-to-image (get-score state)) text-x text-y
                                           (place-pipes (get-pipes state) (place-visible-abilities (ability-types-struct-visible (state-struct-abilities state))
                                                                                                    initial-scene)))))))

; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.

; Coordonata x a gap-ului
(define (get-current-pipe-x pipe)
  (get-pipe-x pipe))

; Coordonata x a noii pozitii
(define (get-x-coordinate pipe)
  (+ (get-current-pipe-x pipe) (quotient pipe-width 2)))

; Coordonata y a gap-ului
(define (get-current-pipe-y pipe)
  (pipe-struct-y-gap pipe))

; Coordonata y a pipe-ului de sus
(define (get-upper-y-coordinte pipe)
  (quotient (get-current-pipe-y pipe) 2))

; Inaltimea pipe-ului de jos
(define (get-lower-pipe-height pipe)
  (- pipe-height (+ pipe-self-gap (get-current-pipe-y pipe))))

; Coordonata y a pipe-ului de jos
(define (get-lower-y-coordinte pipe)
  (+ (get-current-pipe-y pipe) (+ pipe-self-gap (quotient (get-lower-pipe-height pipe) 2))))

(define (place-pipes pipes scene)
  (cond
    [(null? pipes) scene]
    [else
      (let* ([current-pipe (car pipes)]
             [upper-pipe-image (rectangle pipe-width (get-current-pipe-y current-pipe) "solid" "green")]
             [lower-pipe-image (rectangle pipe-width (max 0 (get-lower-pipe-height current-pipe)) "solid" "green")])
        (place-pipes (cdr pipes) (place-image upper-pipe-image (get-x-coordinate current-pipe) (max 0 (get-upper-y-coordinte current-pipe))
                                              (place-image lower-pipe-image (get-x-coordinate current-pipe) (max 0 (get-lower-y-coordinte current-pipe)) scene))))]))

; Bonus
; Completați abilities.rkt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.


; Abilitatea care va accelera timpul va dura 10 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)

; ability-struct (image time pos next)

(define-struct ability-types-struct (visible active) #:transparent)
(define-struct variable-struct (gravity momentum scroll-speed) #:transparent)

(define fast-ability
  (ability-struct (hourglass "mediumseagreen") 10 null
                  (λ (variable)
                    (let ((prev-speed (variable-struct-scroll-speed variable)))
                    (struct-copy variable-struct variable [scroll-speed (max 5 (- prev-speed 1))])))))

; Abilitatea care va încetini timpul va dura 30 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define slow-ability
  (ability-struct (hourglass "tomato") 30 null
                  (λ (variable)
                    (let ((prev-speed (variable-struct-scroll-speed variable)))
                    (struct-copy variable-struct variable [scroll-speed (+ prev-speed 1)])))))
; lista cu toate abilităţile posibile în joc
(define ABILITIES (list slow-ability fast-ability))


(define get-variables
  (λ (state)
    (state-struct-variables state)))

(define get-variables-gravity
  (λ (variable)
    (variable-struct-gravity variable)))

(define get-variables-momentum
  (λ (variable)
    (variable-struct-momentum variable)))

(define get-variables-scroll-speed
  (λ (variable)
    (variable-struct-scroll-speed variable)))

; Întoarce abilităţile din stare, cu o reprezentare
; intermediară care trebuie să conțină două liste:
;  - lista abilităţilor vizibile (încarcate în scenă dar nu neaparat vizibile pe ecran).
;  - lista abilităţilor activate (cu care pasărea a avut o coloziune).
(define (get-abilities state) (state-struct-abilities state))

; Întoarce abilităţile vizibile din reprezentarea intermediară.
(define (get-abilities-visible abilities) (ability-types-struct-visible abilities))

; Întoarce abilităţile active din reprezentarea intermediară.
(define (get-abilities-active abilities) (ability-types-struct-active abilities))

; Șterge din reprezentarea abilităţilor vizibile pe cele care nu mai sunt vizibile.
; echivalent cu clean-pipes.

; Returneaza coltul din dreapta sus
(define (get-upper-right-corner-x ability)
  (+ (posn-x (get-ability-pos ability)) (quotient (image-width (get-ability-image ability)) 2)))

(define (clean-abilities abilities)
  (filter
   (λ (ability)
     (if (> (get-upper-right-corner-x ability) 0) #t
         #f))
     abilities))

; Muta abilităţile vizibile spre stanga.
; echivalent cu move-pipes.

(define (move-abilities abilities scroll-speed)
  (map
   (λ (visi)
       (struct-copy ability-struct visi [pos (make-posn (- (posn-x (ability-struct-pos visi)) scroll-speed)
                                             (posn-y (ability-struct-pos visi)))]))
   abilities))


; Scurge timpul pentru abilităţile activate și le sterge pe cele care au expirat.
; Puteți să va folosiți de variabila globală fps.

; Modific timpul
(define (modify-time abilities)
  (map
   (λ (ability)
     (struct-copy ability-struct ability [time (- (ability-struct-time ability) (/ 1 fps))]))
   abilities))

(define (time-counter abilities)
  (filter
   (λ (ability)
     (if (> (ability-struct-time ability) 0)
         #t
         #f
     ))
  (modify-time abilities)))

; Generează următoarele abilitați vizibile.
; *Atentie* La orice moment pe scena trebuie să fie exact DISPLAYED_ABILITIES
; abilităţi vizibile
; Folosiți funcția fill-abilities din abilities.rkt cât si cele scrise mai sus:
; move-abilities, clean-abilities, time-counter, etc..

; Muta si sterge abilitatile invalide
(define (get-next-visible-state abilities scroll-speed)
  (clean-abilities (move-abilities abilities scroll-speed)))

(define (next-abilities-visible visible scroll-speed)
  (let*
      ((next-state-vis (get-next-visible-state visible scroll-speed)))
    (fill-abilities (clean-abilities (move-abilities visible scroll-speed)) DISPLAYED_ABILITIES ABILITIES)))

; Generează structura intermediară cu abilități.
; Observați ca nu există next-abilities-active aceastea sunt acele abilităti
; întoarse next-abilities-visible care au o coliziune cu pasărea.
; Puteti folosi `filer`/`filter-not` ca sa verificați ce abilităti au și abilitați
; nu au coliziuni cu pasărea sau puteti folosi `partition`

; Gaseste coltul din stanga sus al abilitatii
(define (get-top-left-corner-ability ability)
  (make-posn (- (posn-x (ability-struct-pos ability)) (/ (image-width (ability-struct-image ability)) 2))
             (- (posn-y (ability-struct-pos ability)) (/ (image-height (ability-struct-image ability)) 2))))

; Gaseste coltul din dreapta jos al abilitatii
(define (get-bottom-left-corner-ability ability)
  (make-posn (+ (posn-x (ability-struct-pos ability)) (/ (image-width (ability-struct-image ability)) 2))
             (+ (posn-y (ability-struct-pos ability)) (/ (image-height (ability-struct-image ability)) 2))))

; Coliziune
(define (ability-collisions bird abilities)
    (cond [(null? abilities) #f]       
        [else (cond [(check-collision-rectangles (get-top-right-corner-bird bird) (get-bottom-right-corner-bird bird)
                                              (get-top-left-corner-ability (car (list abilities '()))) (get-bottom-left-corner-ability (car (list abilities)))) #t]
             [else (check-pipe-collisions bird (cdr (list abilities)))])]))

; Alege abilitatile cu care intra in coliziune
(define (abilities-colliding bird abilities)
  (filter
   (λ (ability)
     (if (ability-collisions bird ability)
         #t
         #f))
   abilities))

; Alege abilitatile cu care nu intra in coliziune
(define (abilities-not-colliding bird abilities)
  (filter
   (λ (ability)
     (if (not (ability-collisions bird ability))
         #t
         #f))
   abilities))

; Adauga abilitatile cu care intra in coliziune la cele active
(define (add-active-abilities current-active new-active)
  (append current-active new-active))

; Sterge din abilitatile vizibile pe cele cu care intra in coliziune
(define(remove-visible-abilities visible active)
  (filter
   (λ (ability)
     (if (not (member ability active)) #t
         #f)
    )
   visible))

(define (next-abilities abilities bird scroll-speed)
  (let* ((abilities-collide (abilities-colliding bird (next-abilities-visible (ability-types-struct-visible abilities) scroll-speed)))
         (abilities-visible (abilities-not-colliding bird (next-abilities-visible (ability-types-struct-visible abilities) scroll-speed)))
         (new-active (add-active-abilities (time-counter (ability-types-struct-active abilities)) abilities-collide))
         (new-visible (remove-visible-abilities (next-abilities-visible (ability-types-struct-visible abilities) scroll-speed) new-active)))
    (ability-types-struct new-visible new-active)))

; Dând-use variabilele actuale și abilitațile calculați care vor
; variabile finale folosite în joc
; Folositi compose-abilities
; Atenție când apelați `next-variables` în next-state dați ca paremetru
; initial-variables și nu variabilele aflate deja în stare
; In felul acesta atunci când

(define (next-variables variables abilities)
  (let* ((actives (get-abilities-active abilities)))
   ((compose-abilities
       (map
        (lambda (ability)
          (get-ability-next ability)) actives)) variables)))

; Folosind `place-image/place-images` va poziționa abilităţile vizibile la ability pos.
(define (place-visible-abilities abilities scene)
  (cond
    [(null? abilities) scene]
    [else
      (let* ([current-ability (car abilities)])
        (place-visible-abilities (cdr abilities) (place-image (ability-struct-image current-ability) (posn-x (ability-struct-pos current-ability))
                                                              (posn-y (ability-struct-pos current-ability)) scene)))]))

; Folosind `place-image/place-images` va poziționa abilităţile active
; în partea de sus a ecranului lângă scor.
; Imaginiile vor scalate cu un factor de 0.75 și așezate plecând
; de la ability-posn (constantă globală) cu spații de 50 de px.
; Imaginea cu indexul i va fi așezată la (ability-posn.x - 50*i, ability-posn.y)
(define (place-active-abilities-helper abilities scene i)
  (cond
    [(null? abilities) scene]
    [else
      (let* ([current-ability (car abilities)])
        (place-active-abilities (cdr abilities) (place-image (ability-struct-image current-ability) (- (posn-x abilities-posn) (* 50 i))
                                                              (posn-y abilities-posn) scene) (+ i 1)))]))

(define (place-active-abilities abilities scene)
	(place-active-abilities-helper abilities scene 0))

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
