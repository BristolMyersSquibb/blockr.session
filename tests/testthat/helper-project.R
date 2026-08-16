# Drive the first-save id chooser: open it with Save, then confirm the id.
first_save <- function(session, id, n = 1L) {
  session$setInputs(save_btn = n)
  session$setInputs(rack_id_input = id, rack_id_confirm = n)
}
