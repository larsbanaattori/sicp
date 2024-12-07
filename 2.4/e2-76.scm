; Changes needed to add new types and operations
;
; Generic operations with explicit dispatch:
; - Add new types: a new case needs to be added to every generic operation
; - Add new operation: a new generic operation needs to be added, which means that
;   a version of the operation has to be created for each type being used and then
;   added to the generic operations
;
; Data-directed style
; - Add new types: create the relevant constructors and versions of relevant
;   interface procedures, push to the lookup table. Purely additive.
; - Add new operation: a new version of the operation for each type needs to be
;   pushed to the lookup table
;
; Message-passing style
; - Add new type: create the constructor, which contains the operations
; - Add new operation: modify each constructor
;
; Systems where new types must be added frequently: data-directed or message passing,
; as those are additive with respect to adding new types.
;
; Systems where new operations must be added frequently: generic operations with explicit dispatch
; or data-directed style as both allow the change to be made additively.
