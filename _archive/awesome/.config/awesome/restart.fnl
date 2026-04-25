(local awful (require "awful"))
(local posix (require "posix"))

(require "./table-serialization")
(require "./table-indexof")

;; `layouts` defined in run-init.fnl

;; This save/restore assumes the tags maintain their order.
;; It'd probably be better to match on tag names.

(local tags-state-file (.. (awful.util.get_cache_dir) "/tags-state"))
(local clients-state-file (.. (awful.util.get_cache_dir) "/clients-state"))

(local mod [])

(fn mod.save_state []
  (let [screen _G.mouse.screen tags screen.tags tags-to-restore []]
    (each [i t (ipairs tags)]
      (var sel nil)
      (if (= t.selected true)
          (set sel "true")
          (set sel "false"))
      (table.insert tags-to-restore
                    [i
                     t.name
                     (table.indexof layouts t.layout)
                     t.column_count
                     t.master_width_factor
                     t.master_count
                     sel]))

    (when (posix.stat tags-state-file)
      (os.remove tags-state-file))

    (table.save tags-to-restore tags-state-file)

    (local clients (_G.client.get))
    (local clients-to-restore [])

    (each [i c (ipairs clients)]
      (var ftag nil)
      (when c.first_tag
        (set ftag c.first_tag.name))
      (table.insert clients-to-restore
                    [i c.window c.name ftag]))

    (when (posix.stat clients-state-file)
      (os.remove clients-state-file))

    (table.save clients-to-restore clients-state-file)))

(fn mod.save_state_and_restart []
  (mod.save_state)
  (awesome.restart))

(fn mod.restore_state []
  (when (posix.stat tags-state-file)
    (local tags-to-restore (table.load tags-state-file))
    (local s (awful.screen.focused))
    (each [_ p (ipairs tags-to-restore)]
      (let [[_ name layout ncol mwfact nmaster selected] p
            selected (= selected "true")]

        (var t (awful.tag.find_by_name s name))
        (when (not t)
          (pp {:creating_cached_tag name})
          (awful.tag.add name {:screen s})
          (set t (awful.tag.find_by_name s name)))

        (if t
            (do
              (set t.layout (. layouts layout))
              (set t.column_count ncol)
              (set t.master_width_factor mwfact)
              (set t.master_count nmaster)
              (when (and selected (= t.selected false))
                (awful.tag.viewtoggle t)))
            (pp {:missed_tag_cache_for name})))))

  (when (posix.stat clients-state-file)
    (local s (awful.screen.focused))
    (local clients-to-restore (table.load clients-state-file))
    (each [_ p (ipairs clients-to-restore)]
      (let [[_ window name tag] p]
        (var c nil)

        (each [_ cl (pairs (_G.client.get))]
          (when (= cl.window window)
            (set c cl)))

        (if c
            (when tag
              (local t (awful.tag.find_by_name s tag))
              (when t
                (: c :tags [t])))
            (pp {:missed_cached_client name}))))))

mod

