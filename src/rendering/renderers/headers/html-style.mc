let htmlStylePath = "styles.css"

let htmlStyle: String = 
join ["
/* =========================
   THEME VARIABLES
   ========================= */

:root {
  --bodyBGColor: #f9fafb;
  --bodyTextColor: #111827;
  --h1BorderColor: #d1d5db;
  --h1Color: #111827;
  --h2Color: #1f2937;
  --h2BorderColor: #3b82f6;
  --topDocBorderColor: #d1d5db;
  --topDocColor: #1f2937;
  --topDocBgColor: #ffffff;
  --docBlockBGColor: #ffffff;
  --docBlockBorderColor: #3b82f6;
  --docBlockOutlineColor: #e5e7eb;
  --docSignatureBGColor: #f9f9f9;
  --docDescriptionBGColor: #f0f4ff;
  --docDescriptionBorderColor: #d1d5db;
  --docDescriptionTextColor: #374151;
  --codeBlockBGColor: #fafafa;
  --codeBlockBorderColor: #e5e7eb;
  --toggleHoverBGColor: #bae6fd;
  --gotoLinkColor: #3b82f6;
  --aColor: #2980b9;
  --keywordColor: #dc2626;
  --variableColor: #2563eb;
  --typeColor: #7c3aed;
  --numberColor: #0284c7;
  --commentColor: #16a34a;
  --stringColor: #008000;
  --multiColor: #a0a1a7;
  --searchBarBGColor: #f2f2f2;
  --searchBarTextColor: #111827;
  --searchBarPlaceholderColor: #555;
  --searchBarShadowColor: rgba(0,0,0,0.25);
  --searchResultsBGColor: transparent; /* keep background visible unless needed */
  --searchResultsShadowColor: rgba(0,0,0,0.25);
  --searchResultItemBGColor: #2c2c2c;
  --searchResultItemTextColor: #f5f5f5;
  --searchResultItemHoverBGColor: #3a3a3a;
  --searchResultItemActiveBGColor: #555;
  --searchResultItemBorderColor: rgba(255,255,255,0.08);
  --searchHighlightColor: #4da6ff;

}

/* htmlLight */
:root[data-theme=\"htmlLight\"] {
  --bodyBGColor: #f9fafb;
  --bodyTextColor: #111827;
  --h1BorderColor: #d1d5db;
  --h1Color: #111827;
  --h2Color: #1f2937;
  --h2BorderColor: #3b82f6;
  --topDocBorderColor: #d1d5db;
  --topDocColor: #1f2937;
  --topDocBgColor: #ffffff;
  --docBlockBGColor: #ffffff;
  --docBlockBorderColor: #3b82f6;
  --docBlockOutlineColor: #e5e7eb;
  --docSignatureBGColor: #f9f9f9;
  --docDescriptionBGColor: #f0f4ff;
  --docDescriptionBorderColor: #d1d5db;
  --docDescriptionTextColor: #374151;
  --codeBlockBGColor: #fafafa;
  --codeBlockBorderColor: #e5e7eb;
  --toggleHoverBGColor: #bae6fd;
  --gotoLinkColor: #3b82f6;
  --aColor: #2980b9;
  --keywordColor: #dc2626;
  --variableColor: #2563eb;
  --typeColor: #7c3aed;
  --numberColor: #0284c7;
  --commentColor: #16a34a;
  --stringColor: #008000;
  --multiColor: #a0a1a7;
  --searchBarBGColor: #f2f2f2;
  --searchBarTextColor: #111827;
  --searchBarPlaceholderColor: #6b7280;
  --searchBarShadowColor: rgba(0,0,0,0.15);
  --searchResultsBGColor: #ffffff;
  --searchResultsShadowColor: rgba(0,0,0,0.2);
  --searchResultItemBGColor: #ffffff;
  --searchResultItemTextColor: #1f2937;
  --searchResultItemHoverBGColor: #f3f4f6;
  --searchResultItemActiveBGColor: #e5e7eb;
  --searchResultItemBorderColor: #e5e7eb;
  --searchHighlightColor: #3b82f6;
}

/* htmlDark */
:root[data-theme=\"htmlDark\"] {
  --bodyBGColor: #0d0d0d;
  --bodyTextColor: #e5e5e5;
  --h1BorderColor: #2a2a2a;
  --h1Color: #ffffff;
  --h2Color: #d4d4d4;
  --h2BorderColor: #3b82f6;
  --topDocBorderColor: #2a2a2a;
  --topDocColor: #d0d0d0;
  --topDocBgColor: #121212;
  --docBlockBGColor: #1a1a1a;
  --docBlockBorderColor: #3b82f6;
  --docBlockOutlineColor: #333333;
  --docSignatureBGColor: #222222;
  --docDescriptionBGColor: #121212;
  --docDescriptionBorderColor: #3b82f6;
  --docDescriptionTextColor: #cccccc;
  --codeBlockBGColor: #0f0f0f;
  --codeBlockBorderColor: #2c2c2c;
  --toggleHoverBGColor: #0d0d0d;
  --gotoLinkColor: #3b82f6;
  --aColor: #3b82f6;
  --keywordColor: #f87171;
  --variableColor: #93c5fd;
  --typeColor: #fcd34d;
  --numberColor: #fbbf24;
  --commentColor: #86efac;
  --stringColor: #a8ff60;
  --multiColor: #6a9955;
  --searchBarBGColor: #1f1f1f;
  --searchBarTextColor: #e5e5e5;
  --searchBarPlaceholderColor: #aaaaaa;
  --searchBarShadowColor: rgba(0,0,0,0.4);
  --searchResultsBGColor: #121212;
  --searchResultsShadowColor: rgba(0,0,0,0.6);
  --searchResultItemBGColor: #2c2c2c;
  --searchResultItemTextColor: #f5f5f5;
  --searchResultItemHoverBGColor: #3a3a3a;
  --searchResultItemActiveBGColor: #555555;
  --searchResultItemBorderColor: rgba(255,255,255,0.08);
  --searchHighlightColor: #4da6ff;
}

/* htmlWarm */
:root[data-theme=\"htmlWarm\"] {
  --bodyBGColor: #fef7e0;
  --bodyTextColor: #3b3b2f;
  --h1BorderColor: #f4cd6b;
  --h1Color: #2b2b1f;
  --h2Color: #3a3000;
  --h2BorderColor: #f2b100;
  --topDocBorderColor: #f4cd6b;
  --topDocColor: #3b3b2f;
  --topDocBgColor: #fff9d1;
  --docBlockBGColor: #fffbe6;
  --docBlockBorderColor: #f2b100;
  --docBlockOutlineColor: #fce8b1;
  --docSignatureBGColor: #fff6cf;
  --docDescriptionBGColor: #fdf4c1;
  --docDescriptionBorderColor: #f2d96b;
  --docDescriptionTextColor: #3b3b2f;
  --codeBlockBGColor: #fefce8;
  --codeBlockBorderColor: #f5deb0;
  --toggleHoverBGColor: #ffffff;
  --gotoLinkColor: #e09b00;
  --aColor: #e09b00;
  --keywordColor: #c92a2a;
  --variableColor: #7048e8;
  --typeColor: #d97706;
  --numberColor: #c2410c;
  --commentColor: #5c940d;
  --stringColor: #d19a66;
  --multiColor: #b294bb;
  --searchBarBGColor: #fff9d1;
  --searchBarTextColor: #3b3b2f;
  --searchBarPlaceholderColor: #8c7a3d;
  --searchBarShadowColor: rgba(0,0,0,0.15);
  --searchResultsBGColor: #fffbe6;
  --searchResultsShadowColor: rgba(124, 90, 0, 0.2);
  --searchResultItemBGColor: #fffdf3;
  --searchResultItemTextColor: #3a3000;
  --searchResultItemHoverBGColor: #fef7e0;
  --searchResultItemActiveBGColor: #fce8b1;
  --searchResultItemBorderColor: #f5deb0;
  --searchHighlightColor: #e09b00;
}

/* htmlWarmDark */
:root[data-theme=\"htmlWarmDark\"] {
  --bodyBGColor: #1e1e1e;
  --bodyTextColor: #f5f5dc;
  --h1BorderColor: #facc15;
  --h1Color: #fef9c3;
  --h2Color: #fde68a;
  --h2BorderColor: #facc15;
  --topDocBorderColor: #facc15;
  --topDocColor: #e5e5c0;
  --topDocBgColor: #2a2a1f;
  --docBlockBGColor: #2b2b2b;
  --docBlockBorderColor: #facc15;
  --docBlockOutlineColor: #3f3f3f;
  --docSignatureBGColor: #3a3a3a;
  --docDescriptionBGColor: #343434;
  --docDescriptionBorderColor: #facc15;
  --docDescriptionTextColor: #e7e7c1;
  --codeBlockBGColor: #1c1c1c;
  --codeBlockBorderColor: #333333;
  --toggleHoverBGColor: #1e1e1e;
  --gotoLinkColor: #facc15;
  --aColor: #facc15;
  --keywordColor: #f87171;
  --variableColor: #93c5fd;
  --typeColor: #fcd34d;
  --numberColor: #fbbf24;
  --commentColor: #86efac;
  --stringColor: #ffcb0b;
  --multiColor: #c792e0;
  --searchBarBGColor: #2a2a1f;
  --searchBarTextColor: #f5f5dc;
  --searchBarPlaceholderColor: #c9c9a5;
  --searchBarShadowColor: rgba(0,0,0,0.5);
  --searchResultsBGColor: #2b2b2b;
  --searchResultsShadowColor: rgba(0,0,0,0.7);
  --searchResultItemBGColor: #3a3a3a;
  --searchResultItemTextColor: #fef9c3;
  --searchResultItemHoverBGColor: #444444;
  --searchResultItemActiveBGColor: #555533;
  --searchResultItemBorderColor: #3f3f3f;
  --searchHighlightColor: #facc15;
}

/* =========================
   GENERIC STYLES (use vars)
   ========================= */

body {
  font-family: 'Segoe UI', Roboto, sans-serif;
  background-color: var(--bodyBGColor);
  color: var(--bodyTextColor);
  margin: 2rem auto;
  max-width: 960px;
  padding: 0 1rem;
  font-size: 16px;
  line-height: 1.7;
  letter-spacing: 0.02em;
}

h1 {
  font-size: 2.4em;
  font-weight: 700;
  border-bottom: 3px solid var(--h1BorderColor);
  padding-bottom: 0.4em;
  margin-bottom: 1.5em;
  color: var(--h1Color);
}

h2 {
  font-size: 1.8em;
  font-weight: 600;
  color: var(--h2Color);
  margin-top: 2.5em;
  border-left: 4px solid var(--h2BorderColor);
  padding-left: 0.6em;
}

pre {
  white-space: pre-wrap;
  overflow-wrap: break-word;
}

.top-doc {
  background-color: var(--topDocBgColor);
  border: 1px solid var(--topDocBorderColor);
  border-radius: 6px;
  padding: 1.5rem;
  margin-bottom: 2rem;
  line-height: 1.8;
  font-size: 0.96em;
  color: var(--topDocColor);
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.4);
}

.top-doc pre {
  margin-top: 0.5em;
  margin-bottom: 0.5em;
}

.top-doc code {
  padding: 0.2em 0.4em;
  border-radius: 3px;
  font-family: monospace;
  font-size: 0.95em;
}

.doc-block {
  background-color: var(--docBlockBGColor);
  border-left: 3px solid var(--docBlockBorderColor);
  border: 1px solid var(--docBlockOutlineColor);
  border-radius: 4px;
  padding: 0.8rem 1rem;
  margin: 1.5rem 0;
  font-size: 15px;
  position: relative;
}

.doc-signature {
  background-color: var(--docSignatureBGColor);
  padding: 0.4em 0.8em;
  border-radius: 3px;
  font-weight: 600;
  display: inline-block;
  margin-bottom: 0.6em;
}

.doc-description {
  background-color: var(--docDescriptionBGColor);
  padding: 0.5em 1em;
  color: var(--docDescriptionTextColor);
  font-style: italic;
  font-size: 0.95em;
  margin-bottom: 0.5em;
  white-space: pre-wrap;
  word-wrap: break-word;
  overflow-wrap: break-word;
}

.code-block {
  background-color: var(--codeBlockBGColor);
  border: 1px solid var(--codeBlockBorderColor);
  border-radius: 4px;
  padding: 0.5em 0.8em;
  margin-top: 0.5em;
  font-family: monospace;
  font-size: 0.9em;
}

.toggle-btn {
  all: unset;
  border: none;
  padding: 0.2em 0.5em;
  font-size: 0.85em;
  font-family: monospace;
  cursor: pointer;
  transition: background-color 0.2s ease;
}

.toggle-btn:hover {
  background-color: var(--toggleHoverBGColor);
}

.hiden-code {}

.gotoLink {
  font-size: 1.1em;
  color: var(--gotoLinkColor);
  text-decoration: none;
  position: absolute;
  top: 0.6em;
  right: 1em;
  opacity: 0.7;
  font-weight: 500;
  transition: opacity 0.2s ease, transform 0.2s ease;
}

a {
  color: var(--aColor);
  text-decoration: none;
  font-size: 1.05em;
  font-weight: 500;
  transition: color 0.2s ease, text-decoration 0.2s ease;
}

a:hover {
  text-decoration: underline;
}

.gotoLink:hover {
  opacity: 1;
  transform: scale(1.1);
}

", searchCss, "

.kw      { color: var(--keywordColor); font-weight: 500; }
.var     { color: var(--variableColor); }
.tp      { color: var(--typeColor); }
.number  { color: var(--numberColor); }
.comment { color: var(--commentColor); font-style: italic; }
.string  { color: var(--stringColor); }
.multi   { color: var(--multiColor); }

.theme-toggle {
  position: fixed;
  top: 10px;
  right: 10px;
  padding: 0.4em 0.8em;
  border: 1px solid var(--topDocBorderColor);
  border-radius: 6px;
  font-size: 0.85em;
  font-weight: 500;
  background: var(--docSignatureBGColor);
  color: var(--bodyTextColor);
  cursor: pointer;
  transition: background-color 0.2s ease, opacity 0.2s ease;
  z-index: 1100;
}

.theme-toggle:hover {
  background: var(--toggleHoverBGColor);
  opacity: 0.9;
}

/* Dropdown container */
#themeMenu {
  position: fixed;
  top: 55px;   /* just below the button */
  right: 10px;
  display: none; /* toggled by JS */
  background: var(--topDocBgColor);
  border: 1px solid var(--topDocBorderColor);
  border-radius: 6px;
  box-shadow: 0 4px 8px rgba(0,0,0,0.15);
  padding: 0.4em;
  z-index: 1000;
}

/* Theme option buttons */
#themeMenu button {
  display: block;
  width: 100%;
  padding: 0.4em 0.8em;
  margin: 0.2em 0;
  border: none;
  border-radius: 4px;
  background: var(--docSignatureBGColor);
  color: var(--bodyTextColor);
  cursor: pointer;
  font-size: 0.95em;
  text-align: left;
  transition: background-color 0.2s ease;
}

#themeMenu button:hover {
  background: var(--toggleHoverBGColor);
}
"]
