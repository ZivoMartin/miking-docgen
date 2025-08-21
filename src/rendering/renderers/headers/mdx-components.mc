let mdxJsComponents = ""

let mdxTsComponents =
"import React, { useMemo, useRef, useState, useCallback, createContext, useContext } from 'react';

export const S = {

  body: {
    display: 'grid',
    rowGap: '0.75rem', // uniform spacing between Description / Panels
  },

  card: (compact: boolean) => ({
    border: '1px solid var(--docgen-border, #e5e7eb)',
    borderRadius: 10,
    padding: compact ? '0.4rem 0.6rem' : '0.6rem 0.8rem',
    marginBottom: 30,
    position: 'relative' as const,
    boxShadow: '0 1px 2px rgba(0,0,0,.04)',
    transition: 'box-shadow .15s ease, transform .15s ease, border-color .15s ease',
    willChange: 'transform',
  }),

  header: {
    display: 'flex',
    alignItems: 'baseline',
    gap: '0.8rem',
    flexWrap: 'nowrap',
    marginBottom: '0.8rem', // a bit more breathing room
  },

  title: {
    fontSize: '1.12rem',
    fontWeight: 700,
    letterSpacing: '.2px',
    color: 'var(--docgen-text, #111827)',
    lineHeight: 1,          // keep a clean baseline
  },

  badge: (kind: string) => ({
    display: 'inline-flex',
    alignItems: 'center',
    fontSize: '0.72rem',
    lineHeight: 1,          // match baseline with title
    padding: '0.18rem 0.55rem',
    borderRadius: 9999,
    border: '1px solid var(--docgen-accent, #3b82f6)',
    background:
    'var(--docgen-badge-bg, color-mix(in srgb, var(--docgen-accent, #3b82f6) 8%, transparent))',
    color: 'var(--docgen-accent, #3b82f6)',
    textTransform: 'uppercase' as const,
    letterSpacing: '.3px',
  }),

  desc: {
    color: 'var(--docgen-muted, #6b7280)',
    whiteSpace: 'pre-wrap' as const,
    lineHeight: 1.65,
  },

  spacer: {
    flex: 1, // pushes the link to the far right
  },

  link: {
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.25rem',
    fontSize: '1.1rem',
    fontWeight: 500,
    color: 'var(--docgen-accent, #3b82f6)',
    textDecoration: 'none',
    cursor: 'pointer',
    transition: 'color 0.2s ease'
  },

  panel: {
    border: '1px solid var(--docgen-border, #e5e7eb)',
    borderRadius: 8,
    marginTop: '0.7rem',
    background: 'var(--docgen-surface-2, #f9fafb)',
    overflow: 'hidden'
  },

  panelHeader: {
    display: 'flex',
    gap: '0.5rem',
    padding: '0.5rem 0.65rem',
    background: 'var(--docgen-panel-h, var(--docgen-surface-2, #f9fafb))',
  },

  panelBtn: {
    background: 'transparent',
    border: 'none',
    cursor: 'pointer',
    fontWeight: 500,
    color: 'var(--docgen-text, #111827)',
    letterSpacing: '.2px',
  },

  code: {
    textAlign: 'left',
    fontFamily: 'var(--ifm-font-family-monospace, ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace)',
    fontSize: '0.93rem',
    lineHeight: 1.35,
    tabSize: 2 as any,
  },

  toggler: {
    fontSize: '1.1em',
    textDecoration: 'none',
    opacity: 0.7,
    fontWeight: 500,
    background: 'transparent',
    border: 'none',
    cursor: 'pointer',
  },

  anchor: {
    color: 'inherit',
    textDecoration: 'none',
    position: 'relative',
    '&:hover::after': {
      content: '\"#\"',
      position: 'absolute',
      left: '-1.2rem',
      opacity: 0.6,
    },
  }
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
  link?: string;
  compact?: boolean;
  children: React.ReactNode;
};

export const DocBlock: React.FC<DocBlockProps> = ({ title, kind, link, compact = false, children }) => {
  const [open, setOpen] = useState<PanelState>({});
  const anchorId = useMemo(() => slugify(title), [title]);

  return (
    <Ctx.Provider value={{ open, setOpen, compact }}>
    <section id={anchorId} style={S.card(compact)} aria-labelledby={`${anchorId}-title`}>
      <div style={S.header}>
        <h3 style={S.title} id={`${anchorId}-title`}>
          <a href={`#${anchorId}`} style={S.anchor}>
            {title}
          </a>
        </h3>
        <Badge kind={kind} />
        <div style={S.spacer} />
        {link && (
          <a href={link} style={S.link}>→</a>
        )}
      </div>
      {children}
    </section>
    </Ctx.Provider>
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
        <div style={S.code}>
          {children}
        </div>
      )}
    </div>
  );
};

/** ------------------------------------------------------------------------------------
 *  Minimalist toggle button for top toggling code
 *  ---------------------------------------------------------------------------------- */
type ToggleWrapperProps = { children: React.ReactNode };

export const ToggleWrapper: React.FC<ToggleWrapperProps> = ({ children }) => {
  const [visible, setVisible] = useState(false);
  return (
    <span>
      <button
        onClick={() => setVisible(!visible)}
        style={S.toggler}
        aria-expanded={visible}
      >
        ...
      </button>
      {visible && <span style={{ marginLeft: '0.5rem' }}>{children}</span>}
    </span>
  );
};"
