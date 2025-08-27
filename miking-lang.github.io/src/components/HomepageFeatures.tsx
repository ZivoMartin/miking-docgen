import useBaseUrl from '@docusaurus/useBaseUrl';
import React from 'react';
import clsx from 'clsx';
import styles from './HomepageFeatures.module.css';

type FeatureItem = {
  title: string;
  image: string;
  description: JSX.Element;
};

const FeatureList: FeatureItem[] = [
  {
    title: 'Development',
    image: '/img/development.png',
    description: (
      <>
      The Miking framework is an open-source effort that is currently in Beta status.
      Please visit the <a href="https://github.com/miking-lang/miking"> Github pages </a>
      if you would like to contribute to the development.
      </>
    ),
  },
  {
    title: 'Vision',
    image: '/img/vision.png',
    description: (
      <>
        Our vision is that Miking will become the leading environment for rapid and efficient development of domain-specific languages. Please see the <a href="https://people.kth.se/~dbro/papers/broman-2019-miking-vision.pdf">Miking vision paper</a> for an overview.
      </>
    ),
  },
  {
    title: 'Documentation',
    image: '/img/documentation.png',
    description: (
      <>
        To learn more, please check out the <a href="/docs">online documentation</a>.
      </>
    ),
  },
];

function Feature({title, image, description}: FeatureItem) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <img
          className={styles.featureSvg}
          alt={title}
          src={useBaseUrl(image)}
        />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures(): JSX.Element {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
