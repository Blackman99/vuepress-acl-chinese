<template>
  <div>
  </div>
</template>
<script>
import { watch, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import hljs from 'highlight.js/lib/core'
import lisp from 'highlight.js/lib/languages/lisp'
hljs.registerLanguage('lisp', lisp)

export default {
  setup: () => {
    
    const route = useRoute()

    const setHilight = () => {
      document.querySelectorAll('pre code').forEach(e => e.classList.add('language-lisp'))
      hljs.highlightAll()
    }

    watch(route, (newRoute) => {
      setHilight()
    })

    onMounted(() => {
      setHilight()
    }) 

    return {
      
    }
  }
}
</script>
<style lang="less" scoped>
  
</style>