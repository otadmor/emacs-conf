;;; doom-mhfc-theme.el
(require 'doom-themes)

(defgroup doom-mhfc-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-mhfc-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-mhfc-theme
  :type 'boolean)

(defcustom doom-mhfc-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-mhfc-theme
  :type 'boolean)

(defcustom doom-mhfc-comment-bg doom-mhfc-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-mhfc-theme
  :type 'boolean)

(defcustom doom-mhfc-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-mhfc-theme
  :type '(or integer boolean))

(defcustom doom-mhfc-linum-height 1.1
  "The :height to render line numbers with."
  :group 'doom-mhfc-theme
  :type 'integer)
;;
(def-doom-theme doom-mhfc
  "mhfc theme!"

  ;; name        default   256       16
  ((bg          '("#101010" "#101010" "black"        ))
   (bg-alt      '("#282725" "#282725" "black"        )) ;; arbitrarily picked this colour to change hline
   (base0       '("#2b2a27" "#2b2a27" "grey"         ))
   (base1       '("#1e1e1e" "#1e1e1e" "brightblack"  ))
   (base2       '("#2e2e2e" "#2e2e2e" "brightblack"  ))
   (base3       '("#262626" "#262626" "brightblack"  ))
   (base4       '("#3f3f3f" "#3f3f3f" "brightblack"  ))
   (base5       '("#525252" "#525252" "brightblack"  ))
   (base6       '("#6b6b6b" "#6b6b6b" "brightblack"  ))
   (base7       '("#979797" "#979797" "brightblack"  ))
   (base8       '("#dfdfdf" "#dfdfdf" "white"        ))
   (fg          '("#bfbfbf" "#bfbfbf" "brightwhite"  ))
   (fg-alt      '("#2d2d2d" "#2d2d2d" "white"        ))

   (grey        "base4")
   (white       '("base4"   "base4"   "base4"        ))
   (red         '("#ff6655" "#ff6655" "red"          )) ;; peacock todo 16
   (orange      '("#dd8844" "#dd8844" "brightred"    ))
   (green       '("#98d265" "#98d265" "green"        ))
   (teal        '("#44b9b1" "#44b9b1" "brightgreen"  )) ;; peacock
   (yellow      '("#a0bb38" "#a0bb38" "yellow"       )) ;; peacock, todo 16
   (blue        '("#51afef" "#51afef" "brightblue"   ))
   (dark-blue   '("#2257a0" "#2257a0" "blue"         ))
   (magenta     '("#c678dd" "#c678dd" "magenta"      ))
   (violet      '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan        '("#46d9ff" "#46d9ff" "brightcyan"   ))
   (dark-cyan   '("#5699af" "#5699af" "cyan"         ))
   (coral-popup  '("#f6bfbc" "#f6bfbc" "coral-popup" ))

   ;; face categories -- required for all themes
   (highlight      green)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base1)
   (builtin        green)
   (comments       (if doom-mhfc-brighter-comments dark-cyan base5)) ;; TODO
   (doc-comments   (doom-lighten (if doom-mhfc-brighter-comments dark-cyan base5) 0.25)) ;; TODO
   (constants      red)        ;; done
   (functions      yellow)     ;; done
   (keywords       teal)       ;; done
   (methods        yellow)     ;; not sure how to test this.
   (operators      red)        ;; not showing up on `=` etc.
   (type           white)      ;;
   (strings        yellow)
   (variables      white)      ;; done
   (numbers        red)        ;; done

   (region         `(,(doom-lighten (car bg-alt) 0.05) ,@(doom-lighten (cdr bg-alt) 0.05)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (magit-diff-added-bg             `(,(doom-blend (car green)  (car bg) 0.1) "#262626"))
   (magit-diff-added-highlight-bg   `(,(doom-blend (car green)  (car bg) 0.2) "#303030"))
   (magit-diff-base-bg              `(,(doom-blend (car orange) (car bg) 0.1) "#262626"))
   (magit-diff-base-highlight-bg    `(,(doom-blend (car orange) (car bg) 0.2) "#303030"))
   (magit-diff-hunk-heading-bg      `(,(doom-blend (car violet) (car bg) 0.3) "#262626"))
   (magit-diff-removed-bg           `(,(doom-blend (car red) (car base3) 0.1) "#262626"))
   (magit-diff-removed-highlight-bg `(,(doom-blend (car red) (car base3) 0.2) "#303030"))

   ;; custom categories
   (-modeline-bright doom-mhfc-brighter-modeline)
   (-modeline-pad
    (when doom-mhfc-padded-modeline
      (if (integerp doom-mhfc-padded-modeline) doom-mhfc-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken bg 0.475)
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (linum :foreground (doom-lighten grey 0.14)
          :background "#282828"
          :distant-foreground nil
          :bold nil
          :height doom-mhfc-linum-height)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-mhfc-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base7)

   ;; tooltip
   (tooltip              :background bg-alt :foreground fg)

   ;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-common                           :foreground highlight)
    (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg)
    (company-tooltip-selection  :background selection)
    (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
    (company-tooltip-annotation                       :foreground red)
    (company-scrollbar-bg       :inherit 'tooltip)
    (company-scrollbar-fg       :background highlight)
    (company-preview                                  :foreground highlight)
    (company-preview-common     :background base3 :foreground magenta)
    (company-preview-search     :inherit 'company-tooltip-search)
    (company-template-field     :inherit 'match)

   ;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :inherit 'tooltip)

   ;; pos-tip
   (popup          :inherit 'tooltip)
   (popup-tip-face :inherit 'tooltip)

    ;; mic-paren
    (paren-face-match    :background base0 :weight 'ultra-bold)
    (paren-face-mismatch :foreground base0 :background red   :weight 'ultra-bold)
    (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)


   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))


    ;; ivy
    (ivy-current-match :background region :distant-foreground nil)
    (ivy-minibuffer-match-face-1
     :background nil
     :foreground (doom-lighten grey 0.14)
     :weight 'light)
    (ivy-minibuffer-match-face-2
     :inherit 'ivy-minibuffer-match-face-1
     :foreground red :weight 'light)
    (ivy-minibuffer-match-face-3
     :inherit 'ivy-minibuffer-match-face-2
     :foreground green :weight 'semi-bold)
    (ivy-minibuffer-match-face-4
     :inherit 'ivy-minibuffer-match-face-2
     :foreground yellow :weight 'semi-bold)
    (ivy-minibuffer-match-highlight :foreground red)
    (ivy-highlight-face :foreground red)
    (ivy-confirm-face :foreground success)
    (ivy-match-required-face :foreground error)
    (ivy-virtual :inherit 'italic :foreground doc-comments)
    (ivy-modified-buffer :inherit 'bold :foreground vc-modified)

    ;; ivy-posframe
    (ivy-posframe :background (doom-darken bg-alt 0.2))

    ;; magit
    (magit-diff-added             :foreground (doom-darken green 0.2) :background magit-diff-added-bg             )
    (magit-diff-added-highlight   :foreground green   :weight 'bold   :background magit-diff-added-highlight-bg   )
    (magit-diff-base              :foreground (doom-darken orange 0.2):background magit-diff-base-bg              )
    (magit-diff-base-highlight    :foreground orange  :weight 'bold   :background magit-diff-base-highlight-bg    )
    (magit-diff-hunk-heading      :foreground bg                      :background magit-diff-hunk-heading-bg      )
    (magit-diff-removed           :foreground (doom-darken red 0.2)   :background magit-diff-removed-bg           )
    (magit-diff-removed-highlight :foreground red     :weight 'bold   :background magit-diff-removed-highlight-bg )

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :foreground green) ; :inherit 'bold
   (markdown-code-face :background (doom-lighten base3 0.05)))

  ;; --- extra variables ---------------------
  ;; ()

  ;; TODO
    ;; ;; git-commit
    ;; (git-commit-keyword :foreground cyan :slant 'italic)

    ;; ;; power
    ;; (powerline-active0   :inherit 'mode-line :background bg)
    ;; (powerline-active1   :inherit 'mode-line :background (doom-lighten 'bg 0.025))
    ;; (powerline-active2   :inherit 'mode-line :foreground base8 :background (doom-lighten 'bg 0.08))
    ;; (powerline-inactive0 :inherit 'mode-line-inactive :background base2)
    ;; (powerline-inactive1 :inherit 'mode-line-inactive :background (doom-lighten 'base2 0.02))
    ;; (powerline-inactive2 :inherit 'mode-line-inactive :background (doom-lighten 'base2 0.04))

    ;; ;; whitespace
    ;; (whitespace-tab      :foreground base4 :background (unless (default-value 'indent-tabs-mode) base3))

  )

;;; doom-mhfc-theme.el ends here
(provide 'doom-mhfc-theme)
