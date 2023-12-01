const hljs = require('highlight.js/lib/core')
const lisp = require('highlight.js/lib/languages/lisp')
hljs.registerLanguage('lisp', lisp)


module.exports = {
  lang: 'zh-CN',
  title: 'ANSI Common LISP 中文版',
  description: '使用 Vueperss 搭建的 ANSI Common LISP 中文版，为了更友好的阅读体验',
  head: [
    ['link', { rel: 'manifest', href: '/manifest.webmanifest' }],
    ['meta', { name: 'theme-color', content: '#ba68c8' }],
  ],
  themeConfig: {
    docsBranch: 'master',
    docsDir: 'docs',
    repo: 'https://github.com/Blackman99/vuepress-acl-chinese',
    editLinkText: '编辑此页',
    toggleDarkMode: '切换暗黑模式',
    logo: 'https://common-lisp.net/static/imgs/favicon.ico',
    lastUpdatedText: '最后更新于',
    contributorsText: '贡献者',
    notFound: '页面未找到',
    backToHome: '返回首页',
    openInNewWindow: '在新标签中打开',
    toggleSidebar: '切换侧边栏',
    sidebar: [
      '/acl-chinese-md/preface-cn',
      '/acl-chinese-md/ch1-cn',
      '/acl-chinese-md/ch2-cn',
      '/acl-chinese-md/ch3-cn',
      '/acl-chinese-md/ch4-cn',
      '/acl-chinese-md/ch5-cn',
      '/acl-chinese-md/ch6-cn',
      '/acl-chinese-md/ch7-cn',
      '/acl-chinese-md/ch8-cn',
      '/acl-chinese-md/ch9-cn',
      '/acl-chinese-md/ch10-cn',
      '/acl-chinese-md/ch11-cn',
      '/acl-chinese-md/ch12-cn',
      '/acl-chinese-md/ch13-cn',
      '/acl-chinese-md/ch14-cn',
      '/acl-chinese-md/ch15-cn',
      '/acl-chinese-md/ch16-cn',
      '/acl-chinese-md/ch17-cn',
      '/acl-chinese-md/notes-cn',
      '/acl-chinese-md/appendix-A-cn',
      '/acl-chinese-md/appendix-B-cn',
      '/acl-chinese-md/appendix-C-cn',
      '/acl-chinese-md/appendix-D-cn', 
    ]
  },
  extendsMarkdown: (md) => {
    md.renderer.rules.code_block = function (tokens, idx, options, env, slf) {
      const token = tokens[idx]

      return `<pre ${slf.renderAttrs(token)}><code>${hljs.highlight(token.content, { language: 'lisp' }).value}</code></pre>`
    
    }
  },
  plugins: [
    [
      '@vuepress/pwa',
      {
        skipWaiting: true,
      },
    ],
    [
      '@vuepress/plugin-pwa-popup',
      {
        locales: {
          '/': {
            message: '发现新内容可用',
            buttonText: '刷新',
          },
        },
      },
    ],
    [
      '@vuepress/plugin-google-analytics',
      {
        id: 'G-SC146SMYL1',
      },
    ],
  ],
}
