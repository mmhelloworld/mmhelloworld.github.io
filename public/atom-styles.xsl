<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:a="http://www.w3.org/2005/Atom"
  exclude-result-prefixes="a">
  <xsl:output method="html" version="1.0" encoding="UTF-8" indent="yes" />
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
      <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width,initial-scale=1" />
        <title><xsl:value-of select="/a:feed/a:title" /> • Atom</title>
        <meta name="color-scheme" content="light dark" />
        <style>
          :root {
            --c-bg: #f5f5f5;
            --c-card: #fff;
            --c-text: #2c3e50;
            --c-text2: #7f8c8d;
            --c-accent: #3b82f6;
            --c-border: #e8e8e8;
            --radius: 12px;
          }
          @media (prefers-color-scheme: dark) {
            :root {
              --c-bg: #0a0a0a;
              --c-card: #161616;
              --c-text: #e8e8e8;
              --c-text2: #999;
              --c-border: #2a2a2a;
            }
          }
          * { box-sizing: border-box; margin: 0; padding: 0; }
          body {
            font: 15px/1.75 "Inter","Noto Sans SC",-apple-system,Segoe UI,Roboto,sans-serif;
            background: var(--c-bg); color: var(--c-text);
            max-width: 720px; margin: 0 auto; padding: 40px 20px 80px;
          }
          .header {
            text-align: center; padding: 48px 0 40px;
            border-bottom: 1px solid var(--c-border); margin-bottom: 36px;
          }
          .header h1 { font-size: 1.75rem; font-weight: 700; margin-bottom: 6px; }
          .header p { font-size: .95rem; color: var(--c-text2); }
          .header .pill {
            display: inline-block; margin-top: 14px;
            background: var(--c-accent); color: #fff; font-size: .8rem;
            font-weight: 600; padding: 4px 16px; border-radius: 20px;
          }
          .header .meta { margin-top: 14px; font-size: .82rem; color: var(--c-text2); }
          .item {
            background: var(--c-card); border: 1px solid var(--c-border);
            border-radius: var(--radius); padding: 20px 24px; margin-bottom: 14px;
            transition: transform .2s, box-shadow .2s;
          }
          .item:hover {
            transform: translateY(-1px);
            box-shadow: 0 4px 20px rgba(59,130,246,.12);
          }
          .item .date { font-size: .78rem; color: var(--c-text2); margin-bottom: 4px; }
          .item .title {
            font-size: 1.05rem; font-weight: 600; color: var(--c-text);
            text-decoration: none; line-height: 1.4;
          }
          .item .title:hover { color: var(--c-accent); }
          .item .desc { font-size: .85rem; color: var(--c-text2); margin-top: 6px; line-height: 1.6; }
          .item .link-row { margin-top: 10px; }
          .item .link-row a { font-size: .8rem; color: var(--c-accent); text-decoration: none; font-weight: 500; }
          .item .link-row a:hover { text-decoration: underline; }
          .footer {
            text-align: center; margin-top: 44px; padding-top: 24px;
            border-top: 1px solid var(--c-border);
            font-size: .82rem; color: var(--c-text2);
          }
          .footer a { color: var(--c-accent); text-decoration: none; }
          .footer a:hover { text-decoration: underline; }
        </style>
      </head>
      <body>
        <div class="header">
          <h1><xsl:value-of select="/a:feed/a:title" /></h1>
          <p><xsl:value-of select="/a:feed/a:subtitle" /></p>
          <div class="pill">Atom Feed</div>
          <div class="meta">
            <xsl:value-of select="count(/a:feed/a:entry)" /> posts •
            <xsl:value-of select="substring(/a:feed/a:updated, 1, 10)" />
          </div>
        </div>

        <xsl:for-each select="/a:feed/a:entry">
          <div class="item">
            <div class="date">
              <xsl:value-of select="substring(a:updated, 1, 10)" />
            </div>
            <a class="title" href="{a:link[not(@rel) or @rel='alternate']/@href}">
              <xsl:value-of select="a:title" />
            </a>
            <xsl:if test="a:summary and a:summary != ''">
              <div class="desc">
                <xsl:value-of select="a:summary" />
              </div>
            </xsl:if>
            <div class="link-row">
              <a href="{a:link[not(@rel) or @rel='alternate']/@href}">Read more →</a>
            </div>
          </div>
        </xsl:for-each>

        <div class="footer">
          <p>
            <xsl:value-of select="count(/a:feed/a:entry)" /> posts ·
            <a href="{/a:feed/a:link[@rel='alternate']/@href}">
              <xsl:value-of select="/a:feed/a:title" />
            </a>
          </p>
          <p style="margin-top:6px">
            Subscribe with an <a href="https://aboutfeeds.com" target="_blank" rel="noopener">Atom/RSS reader</a>
          </p>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
