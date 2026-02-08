# Recruiting Agent

Der Recruiting Agent ist ein intelligenter KI-Assistent für Recruiting, powered by OpenAI ChatKit, entwickelt mit Next.js und optimiert für Vercel Deployment.

🚀 **Live at**: https://agents-thesis-lara-hft-v1-sepia.vercel.app

## Features

- 🤖 Intelligente KI-Konversationen powered by OpenAI ChatKit
- 🎨 Schöne, responsive UI mit Dark/Light Theme Unterstützung
- 📁 Datei-Upload für verbesserte Interaktionen
- 🔄 Echtzeit-Streaming Antworten
- 📱 Mobile-freundliches Design
- ⚡ Edge Runtime für optimale Performance

## Quick Start

### Prerequisites

- Node.js 18+ 
- OpenAI API key
- ChatKit workflow ID

### Local Development

1. Repository klonen:
```bash
git clone <your-repo-url>
cd recruiting-agent
```

2. Install dependencies:
```bash
npm install
```

3. Create environment file:
```bash
cp env.example .env.local
```

4. Configure environment variables in `.env.local`:
```env
OPENAI_API_KEY=your_openai_api_key_here
NEXT_PUBLIC_CHATKIT_WORKFLOW_ID=wf_your_workflow_id_here
```

5. Start the development server:
```bash
npm run dev
```

6. Open [http://localhost:3000](http://localhost:3000) in your browser.

## Vercel Deployment

### Method 1: Deploy with Vercel CLI

1. Install Vercel CLI:
```bash
npm i -g vercel
```

2. Login to Vercel:
```bash
vercel login
```

3. Deploy:
```bash
vercel
```

4. Set environment variables in Vercel dashboard:
   - `OPENAI_API_KEY`: Your OpenAI API key
   - `NEXT_PUBLIC_CHATKIT_WORKFLOW_ID`: Your ChatKit workflow ID

### Method 2: Deploy via Vercel Dashboard

1. Push your code to GitHub/GitLab/Bitbucket
2. Connect your repository to Vercel
3. Set environment variables in project settings:
   - `OPENAI_API_KEY`: Your OpenAI API key
   - `NEXT_PUBLIC_CHATKIT_WORKFLOW_ID`: Your ChatKit workflow ID
4. Deploy!

## Environment Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `OPENAI_API_KEY` | Your OpenAI API key | Yes |
| `NEXT_PUBLIC_CHATKIT_WORKFLOW_ID` | ChatKit workflow ID (starts with `wf_`) | Yes |
| `CHATKIT_API_BASE` | Custom ChatKit API base URL (optional) | No |

## Getting Your ChatKit Workflow ID

1. Go to [OpenAI Platform](https://platform.openai.com)
2. Navigate to ChatKit section
3. Create a new workflow or use an existing one
4. Copy the workflow ID (starts with `wf_`)

## Customization

### Branding
- Update the greeting message in `lib/config.ts`
- Modify starter prompts in `lib/config.ts`
- Change the page title in `app/layout.tsx`

### Styling
- Customize themes in `lib/config.ts`
- Modify global styles in `app/globals.css`
- Update component styles in `components/`

## Tech Stack

- **Framework**: Next.js 15
- **UI Library**: React 19
- **AI Integration**: OpenAI ChatKit
- **Styling**: Tailwind CSS
- **TypeScript**: Full type safety
- **Deployment**: Vercel (optimized)

## License

MIT License - see LICENSE file for details.

## Support

For issues and questions:
- Check the [OpenAI ChatKit documentation](https://platform.openai.com/docs/guides/chatkit)
- Review the [Next.js documentation](https://nextjs.org/docs)
- Open an issue in this repository