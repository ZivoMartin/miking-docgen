let mdxComponents = 
"
import React from 'react';

type HighlightProps = {
  children: React.ReactNode;
  color: string;
};

export const Highlight = ({ children, color }: HighlightProps) => (
  <span style={{ color }}>{children}</span>
);

type Props = { children: React.ReactNode };

export const Kw = ({ children }: Props) => <Highlight color="#dc2626">{children}</Highlight>;
export const Var = ({ children }: Props) => <Highlight color="#2563eb">{children}</Highlight>;
export const Tp = ({ children }: Props) => <Highlight color="#7c3aed">{children}</Highlight>;
export const Num = ({ children }: Props) => <Highlight color="#0284c7">{children}</Highlight>;
export const Comment = ({ children }: Props) => <Highlight color="#16a34a">{children}</Highlight>;
export const Str = ({ children }: Props) => <Highlight color="#008000">{children}</Highlight>;
export const Multi = ({ children }: Props) => <Highlight color="#a0a1a7">{children}</Highlight>;


type ToggleWrapperProps = {
  children: React.ReactNode;
};

export const ToggleWrapper: React.FC<ToggleWrapperProps> = ({ children }) => {
  const [visible, setVisible] = useState(false);

  return (
    <div>
      <button onClick={() => setVisible(!visible)}>...</button>
      {visible && <div>{children}</div>}
    </div>
  );
};
"
