<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:atom="http://www.w3.org/2005/Atom"
  xmlns:dc="http://purl.org/dc/elements/1.1/"
  xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd">
  <xsl:output method="html" version="1.0" encoding="UTF-8" indent="yes" />
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="zh-CN">
      <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width,initial-scale=1" />
        <title><xsl:value-of select="/rss/channel/title" /> • RSS</title>
        <meta name="color-scheme" content="light dark" />
        <style>
          :root {
            --c-bg: #f5f5f5;
            --c-card: #fff;
            --c-text: #2c3e50;
            --c-text2: #7f8c8d;
            --c-accent: #e9536a;
            --c-accent-bg: #fef2f3;
            --c-border: #e8e8e8;
            --radius: 12px;
          }
          @media (prefers-color-scheme: dark) {
            :root {
              --c-bg: #1a1a2e;
              --c-card: #1e2a45;
              --c-text: #e8e8e8;
              --c-text2: #999;
              --c-accent-bg: #2a1a22;
              --c-border: #2a2a4a;
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
          .header h1 {
            font-size: 1.75rem; font-weight: 700; margin-bottom: 6px;
          }
          .header p {
            font-size: .95rem; color: var(--c-text2);
          }
          .header .pill {
            display: inline-block; margin-top: 14px;
            background: var(--c-accent); color: #fff; font-size: .8rem;
            font-weight: 600; padding: 4px 16px; border-radius: 20px;
          }
          .header .meta {
            margin-top: 14px; font-size: .82rem; color: var(--c-text2);
          }
          .item {
            background: var(--c-card); border: 1px solid var(--c-border);
            border-radius: var(--radius); padding: 20px 24px; margin-bottom: 14px;
            transition: transform .2s, box-shadow .2s;
          }
          .item:hover {
            transform: translateY(-1px);
            box-shadow: 0 4px 20px rgba(233,83,106,.08);
          }
          .item .date {
            font-size: .78rem; color: var(--c-text2); margin-bottom: 4px;
          }
          .item .title {
            font-size: 1.05rem; font-weight: 600; color: var(--c-text);
            text-decoration: none; line-height: 1.4;
          }
          .item .title:hover { color: var(--c-accent); }
          .item .desc {
            font-size: .85rem; color: var(--c-text2); margin-top: 6px;
            line-height: 1.6;
          }
          .item .link-row {
            margin-top: 10px;
          }
          .item .link-row a {
            font-size: .8rem; color: var(--c-accent); text-decoration: none;
            font-weight: 500;
          }
          .item .link-row a:hover { text-decoration: underline; }
          .footer {
            text-align: center; margin-top: 44px; padding-top: 24px;
            border-top: 1px solid var(--c-border);
            font-size: .82rem; color: var(--c-text2);
          }
          .footer a { color: var(--c-accent); text-decoration: none; }
          .footer a:hover { text-decoration: underline; }
          .stats {
            display: flex; gap: 24px; justify-content: center;
            margin: 16px 0 8px; font-size: .82rem; color: var(--c-text2);
          }
          .stats span { font-weight: 600; color: var(--c-text); }
        </style>
      </head>
      <body>
        <div class="header">
          <h1><xsl:value-of select="/rss/channel/title" /></h1>
          <p><xsl:value-of select="/rss/channel/description" /></p>
          <div class="pill">RSS Feed</div>
          <div class="meta">
            <xsl:value-of select="count(/rss/channel/item)" /> 篇文章 •
            <xsl:value-of select="/rss/channel/lastBuildDate" />
          </div>
        </div>

        <xsl:for-each select="/rss/channel/item">
          <xsl:sort select="substring(pubDate, 13, 2) * 3600 + substring(pubDate, 16, 2) * 60 + substring(pubDate, 19, 2)" order="descending" data-type="number" />
          <xsl:sort select="concat(
            substring(pubDate, 9, 2), '/',
            substring(pubDate, 6, 3), '/',
            substring(pubDate, 1, 4)
          )" order="descending" />
          <div class="item">
            <div class="date">
              <xsl:value-of select="pubDate" />
            </div>
            <a class="title" href="{link}">
              <xsl:value-of select="title" />
            </a>
            <xsl:if test="description and description != ''">
              <div class="desc">
                <xsl:value-of select="description" />
              </div>
            </xsl:if>
            <div class="link-row">
              <a href="{link}">阅读全文 →</a>
            </div>
          </div>
        </xsl:for-each>

        <div class="footer">
          <p>
            <xsl:value-of select="count(/rss/channel/item)" /> 篇文章 ·
            <a>
              <xsl:attribute name="href"><xsl:value-of select="/rss/channel/link" /></xsl:attribute>
              <xsl:value-of select="/rss/channel/title" />
            </a>
          </p>
          <p style="margin-top:6px">
            通过 <a href="https://aboutfeeds.com" target="_blank" rel="noopener">RSS 阅读器</a> 订阅本博客
          </p>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
