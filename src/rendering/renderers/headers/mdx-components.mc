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
import React, { useState } from 'react';


type ToggleWrapperProps = {
  children: React.ReactNode;
};

export const ToggleWrapper: React.FC<ToggleWrapperProps> = ({ children }) => {
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
