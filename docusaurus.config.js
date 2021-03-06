const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');
const math = require('remark-math');
const katex = require('rehype-katex');

/** @type {import('@docusaurus/types').DocusaurusConfig} */
module.exports = {
  title: 'Yet another SICP website',
  tagline: '',
  url: 'https://yet-another-sicp-website.com',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'jugheadjones10', // Usually your GitHub org/user name.
  projectName: 'yet-another-SICP-website', // Usually your repo name.
  themeConfig: {
    navbar: {
      title: 'Yet another SICP website',
      logo: {
        alt: 'My Site Logo',
        src: 'img/logo.svg',
      },
      items: [
//        {
//          type: 'doc',
//          docId: 'intro',
//          position: 'left',
//          label: 'Exercises',
//        },
//        {to: '/blog', label: 'Blog', position: 'left'},
//        {
//          href: 'https://github.com/facebook/docusaurus',
//          label: 'GitHub',
//          position: 'right',
//        },
      ],
    },
//    footer: {
//      style: 'dark',
//      links: [
//        {
//          title: 'Docs',
//          items: [
//           // {
//           //   label: 'Tutorial',
//           //   to: '/docs/intro',
//           // },
//          ],
//        },
//        {
//          title: 'Community',
//          items: [
//            {
//              label: 'Stack Overflow',
//              href: 'https://stackoverflow.com/questions/tagged/docusaurus',
//            },
//            {
//              label: 'Discord',
//              href: 'https://discordapp.com/invite/docusaurus',
//            },
//            {
//              label: 'Twitter',
//              href: 'https://twitter.com/docusaurus',
//            },
//          ],
//        },
////        {
////          title: 'More',
////          items: [
////            {
////              label: 'Blog',
////              to: '/blog',
////            },
////            {
////              label: 'GitHub',
////              href: 'https://github.com/facebook/docusaurus',
////            },
////          ],
////        },
//      ],
//      copyright: `Copyright ?? ${new Date().getFullYear()} My Project, Inc. Built with Docusaurus.`,
//    },
    prism: {
      theme: lightCodeTheme,
      darkTheme: darkCodeTheme,
additionalLanguages: ['scheme']
    },
  },
	stylesheets: [
	    {
		            href: "https://cdn.jsdelivr.net/npm/katex@0.13.11/dist/katex.min.css",
		            integrity: "sha384-Um5gpz1odJg5Z4HAmzPtgZKdTBHZdw8S29IecapCSB31ligYPhHQZMIlWLYQGVoc",
		            crossorigin: "anonymous",
		        },
	],
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl:
            'https://github.com/facebook/docusaurus/edit/master/website/',
		routeBasePath: '/',
		remarkPlugins: [math],
		rehypePlugins: [katex],
        },
//        blog: {
//          showReadingTime: true,
//          // Please change this to your repo.
//          editUrl:
//            'https://github.com/facebook/docusaurus/edit/master/website/blog/',
//        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};
