let mdxJsComponents =
"
import React, { useState } from 'react';

export const ToggleWrapper = ({ children }) => {
  const [visible, setVisible] = useState(false);

  return (
    <span>
      <button
        onClick={() => setVisible(!visible)}
        style={{
          fontSize: \"1.1em\",
          textDecoration: \"none\",
          top: \"0.6em\",
          right: \"1em\",
          opacity: 0.7,
          fontWeight: 500,
          transition: \"opacity 0.2s ease, transform 0.2s ease\",
          background: \"transparent\",
          border: \"none\",
          cursor: \"pointer\",
        }}
      >
        ...
      </button>
      {visible && <span style={{ marginLeft: \"0.5em\" }}>{children}</span>}
    </span>
  );
};
    
"


let mdxTsComponents = 
"
import React, { useMemo, useRef, useState, useCallback, createContext, useContext } from 'react';

const S = {
  card: (compact: boolean) => ({
    border: '1px solid var(--docgen-border, #e5e7eb)',
    borderRadius: 10,
    margin: '0.5rem 0',
    padding: compact ? '0.4rem 0.6rem' : '0.6rem 0.8rem',
    background: 'var(--docgen-surface, #ffffff)',
    position: 'relative' as const,
    boxShadow: '0 1px 2px rgba(0,0,0,.04)',
    transition: 'box-shadow .15s ease, transform .15s ease, border-color .15s ease',
    willChange: 'transform',
  }),

  header: {

    gap: '0.6rem',
    flexWrap: 'wrap' as const,
    marginBottom: '0.5rem',
    paddingBottom: '1.0rem',
    borderBottom: '1px solid var(--docgen-border, #e5e7eb)',
  },

  title: {
    fontSize: '1.06rem',
    fontWeight: 700,
    letterSpacing: '.2px',
    margin: 0,
    color: 'var(--docgen-text, #111827)',
  },

  badge: (kind: string) => ({
    fontSize: '0.72rem',
    padding: '0.18rem 0.55rem',
    borderRadius: 9999,
    border: '1px solid var(--docgen-accent, #3b82f6)',
    background: 'var(--docgen-badge-bg, color-mix(in srgb, var(--docgen-accent, #3b82f6) 8%, transparent))',
    color: 'var(--docgen-accent, #3b82f6)',
    textTransform: 'uppercase' as const,
    letterSpacing: '.3px',
    lineHeight: 1.2,
  }),

  actions: {
    marginLeft: 'auto',
    display: 'flex',
    gap: '0.4rem',
    flexWrap: 'wrap' as const,
  },

  actionBtn: {
    fontSize: '0.82rem',
    padding: '0.32rem 0.6rem',
    borderRadius: 8,
    border: '1px solid var(--docgen-border, #e5e7eb)',
    background: 'transparent',
    color: 'var(--docgen-text, #111827)',
    cursor: 'pointer',
    transition: 'background-color .15s ease, border-color .15s ease, transform .1s ease',
  },

  desc: {
    marginTop: '0.65rem',
    marginBottom: '0.25rem',
    color: 'var(--docgen-muted, #6b7280)',
    whiteSpace: 'pre-wrap' as const,
    lineHeight: 1.65,
  },

  panel: {
    border: '1px solid var(--docgen-border, #e5e7eb)',
    borderRadius: 8,
    marginTop: '0.7rem',
    overflow: 'hidden' as const,
    background: 'var(--docgen-surface-2, #f9fafb)',
  },

  panelHeader: {
    display: 'flex',
    gap: '0.5rem',
    padding: '0.5rem 0.65rem',
    background: 'var(--docgen-panel-h, var(--docgen-surface-2, #f9fafb))',
    borderBottom: '1px solid var(--docgen-border, #e5e7eb)',
  },

  chevron: (open: boolean) => ({
    display: 'inline-block',
    transform: `rotate(${open ? 90 : 0}deg)`,
    transition: 'transform .15s ease',
    opacity: .9,
  }),

  panelBtn: {
    background: 'transparent',
    border: 'none',
    cursor: 'pointer',
    fontWeight: 650,
    color: 'var(--docgen-text, #111827)',
    letterSpacing: '.2px',
  },

  codeWrap: {
    padding: '0.3rem 0.4rem',
    background: 'var(--docgen-code-bg, #f8fafc)',
  },

  code: {
    fontFamily: 'var(--ifm-font-family-monospace, ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace)',
    fontSize: '0.93rem',
    lineHeight: 1.35,
    tabSize: 2 as any,
  },

  permalink: {
    textDecoration: 'none',
    color: 'var(--docgen-accent, #3b82f6)',
    padding: '0 0.35rem',
    opacity: .85,
  },
};

/** ------------------------------------------------------------------------------------
 *  Utils
 *  ---------------------------------------------------------------------------------- */
function slugify(input: string) {
  return input
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/(^-|-$)/g, '');
}

function useId(prefix?: string) {
  const ref = useRef<string>();
  if (!ref.current) {
    const rnd = Math.random().toString(36).slice(2, 8);
    ref.current = `${prefix ?? 'docgen'}-${rnd}`;
  }
  return ref.current;
}

/** ------------------------------------------------------------------------------------
 *  Context : Pannels gestion
 *  ---------------------------------------------------------------------------------- */
type PanelState = Record<string, boolean>;

type DocBlockCtx = {
  open: PanelState;
  setOpen: React.Dispatch<React.SetStateAction<PanelState>>;
  compact: boolean;
  href?: string;
};

const Ctx = createContext<DocBlockCtx | null>(null);

function useDocBlockCtx() {
  const ctx = useContext(Ctx);
  if (!ctx) throw new Error('DocBlock context missing. Place Action/Panel inside <DocBlock>.');
  return ctx;
}

/** ------------------------------------------------------------------------------------
 *  Badge
 *  ---------------------------------------------------------------------------------- */
export const Badge: React.FC<{ kind?: string }> = ({ kind }) => {
  if (!kind) return null;
  return <span style={S.badge(kind)}>{kind}</span>;
};

/** ------------------------------------------------------------------------------------
 *  DocBlock
 *  ---------------------------------------------------------------------------------- */
type DocBlockProps = {
  title: string;
  kind?: string;
  href?: string;
  compact?: boolean;
  children: React.ReactNode;
};

export const DocBlock: React.FC<DocBlockProps> = ({ title, kind, href, compact = false, children }) => {
  const [open, setOpen] = useState<PanelState>({});
  const anchorId = useMemo(() => slugify(title), [title]);

  return (
    <Ctx.Provider value={{ open, setOpen, compact, href }}>
      <section id={anchorId} style={S.card(compact)} aria-labelledby={`${anchorId}-title`}>
        <div style={S.header}>
          <h3 style={S.title} id={`${anchorId}-title`}>{title}</h3>
          <Badge kind={kind} />
          <div style={S.actions}>
            {/* Les Actions (boutons) peuvent aussi être déclarées par l'utilisateur */}
          </div>
        </div>
        {children}
      </section>
    </Ctx.Provider>
  );
};

/** ------------------------------------------------------------------------------------
 *  Actions wrapper
 *  ---------------------------------------------------------------------------------- */
export const Actions: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  return <div style={{ ...S.actions, marginTop: 4 }}>{children}</div>;
};

/** ------------------------------------------------------------------------------------
 *  ActionToggle
 *  ---------------------------------------------------------------------------------- */
type ActionToggleProps = {
  target: string;                 // id de panneau
  labelShow?: string;
  labelHide?: string;
};

export const ActionToggle: React.FC<ActionToggleProps> = ({ target, labelShow = 'Display', labelHide = 'Hide' }) => {
  const { open, setOpen } = useDocBlockCtx();
  const isOpen = !!open[target];
  const onClick = useCallback(() => {
    setOpen(prev => ({ ...prev, [target]: !prev[target] }));
  }, [setOpen, target]);

  return (
    <button
      type=\"button\"
      aria-expanded={isOpen}
      aria-controls={target}
      onClick={onClick}
      title={isOpen ? labelHide : labelShow}
      style={S.actionBtn}
    >
      {isOpen ? labelHide : labelShow}
    </button>
  );
};

/** ------------------------------------------------------------------------------------
 *  ActionCopy
 *  ---------------------------------------------------------------------------------- */
type ActionCopyProps = {
  code: string;
  label?: string;
};

export const ActionCopy: React.FC<ActionCopyProps> = ({ code, label = 'Copy' }) => {
  const [ok, setOk] = useState(false);
  const onCopy = async () => {
    try {
      await navigator.clipboard.writeText(code);
      setOk(true);
      setTimeout(() => setOk(false), 1200);
    } catch {
      // ignore
    }
  };
  return (
    <button type=\"button\" onClick={onCopy} style={S.actionBtn} aria-live=\"polite\">
      {ok ? '✔ Copied' : label}
    </button>
  );
};

/** ------------------------------------------------------------------------------------
 *  ActionPermalink
 *  ---------------------------------------------------------------------------------- */
export const ActionPermalink: React.FC<{ href?: string; label?: string }> = ({ href, label = 'Permalien' }) => {
  if (!href) return null;
  return (
    <a href={href} style={S.permalink} title={label} aria-label={label}>
      ↗
    </a>
  );
};


/** ------------------------------------------------------------------------------------
 *  Description
 *  ---------------------------------------------------------------------------------- */
export const Description: React.FC<{ children?: React.ReactNode }> = ({ children }) => {
  if (!children) return null;
  return <div style={S.desc}>{children}</div>;
};

/** ------------------------------------------------------------------------------------
 *  Panel
 *  ---------------------------------------------------------------------------------- */
type PanelProps = {
  id?: string;               
  title?: string;            
  defaultOpen?: boolean;
  children: string;          
};

export const Panel: React.FC<PanelProps> = ({ id, title = 'Détails', defaultOpen = false, children }) => {
  const autoId = useId('panel');
  const panelId = id ?? autoId;
  const { open, setOpen } = useDocBlockCtx();
  const isOpen = open[panelId] ?? defaultOpen;

  React.useEffect(() => {
    setOpen(prev => (panelId in prev ? prev : { ...prev, [panelId]: defaultOpen }));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [panelId]);

  const toggle = () => setOpen(prev => ({ ...prev, [panelId]: !prev[panelId] }));

  return (
    <div style={S.panel}>
      <div style={S.panelHeader}>
        <button
          type=\"button\"
          onClick={toggle}
          aria-expanded={isOpen}
          aria-controls={panelId}
          style={S.panelBtn}
        >
          {title}
        </button>
      </div>
      {isOpen && (
        <div id={panelId} role=\"region\" style={S.codeWrap}>
          <pre style={{ margin: 0 }}>
            <code className=\"language-mc\" style={S.code}>{children}</code>
          </pre>
        </div>
      )}
    </div>
  );
};

/** ------------------------------------------------------------------------------------
 *  Compat : ancien ToggleWrapper (garde-le pour ne pas casser l’existant)
 *  ---------------------------------------------------------------------------------- */
type ToggleWrapperProps = { children: React.ReactNode };

export const ToggleWrapper: React.FC<ToggleWrapperProps> = ({ children }) => {
  const [visible, setVisible] = useState(false);
  return (
    <span>
      <button
        onClick={() => setVisible(!visible)}
        style={{
          fontSize: '1.1em',
          textDecoration: 'none',
          opacity: 0.7,
          fontWeight: 500,
          background: 'transparent',
          border: 'none',
          cursor: 'pointer',
        }}
        aria-expanded={visible}
      >
        ...
      </button>
      {visible && <span style={{ marginLeft: '0.5rem' }}>{children}</span>}
    </span>
  );
};
"
    
