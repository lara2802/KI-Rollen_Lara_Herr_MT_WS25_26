# Vercel Deployment Guide für Recruiting Agent

Dieser Guide hilft Ihnen beim Deployment des Recruiting Agent auf Vercel.

## Prerequisites

1. **OpenAI API Key**: Get one from [OpenAI Platform](https://platform.openai.com/api-keys)
2. **ChatKit Workflow ID**: Create a workflow at [OpenAI ChatKit](https://platform.openai.com/chatkit)
3. **Vercel Account**: Sign up at [vercel.com](https://vercel.com)

## Step 1: Prepare Your Repository

1. Push your code to GitHub, GitLab, or Bitbucket
2. Make sure all files are committed and pushed

## Step 2: Deploy to Vercel

### Option A: Deploy via Vercel Dashboard (Recommended)

1. Go to [vercel.com](https://vercel.com) and sign in
2. Click "New Project"
3. Import your repository
4. Vercel will automatically detect it's a Next.js project
5. Click "Deploy"

### Option B: Deploy via Vercel CLI

1. Install Vercel CLI:
```bash
npm i -g vercel
```

2. Login to Vercel:
```bash
vercel login
```

3. Navigieren Sie zu Ihrem Projektverzeichnis:
```bash
cd recruiting-agent
```

4. Deploy:
```bash
vercel
```

5. Follow the prompts to configure your project

## Step 3: Configure Environment Variables

After deployment, you need to set up environment variables:

1. Go to your project dashboard in Vercel
2. Navigate to "Settings" → "Environment Variables"
3. Add the following variables:

| Variable | Value | Description |
|----------|-------|-------------|
| `OPENAI_API_KEY` | `sk-...` | Your OpenAI API key |
| `NEXT_PUBLIC_CHATKIT_WORKFLOW_ID` | `wf_...` | Your ChatKit workflow ID |

4. Click "Save" for each variable

## Step 4: Redeploy

After adding environment variables:

1. Go to "Deployments" tab
2. Click "Redeploy" on the latest deployment
3. Wait for the deployment to complete

## Step 5: Test Your Deployment

1. Besuchen Sie Ihre deployed URL (z.B., `https://recruiting-agent-xyz.vercel.app`)
2. Sie sollten die Recruiting Agent Oberfläche sehen
3. Stellen Sie eine Frage um die Funktionalität zu testen

## Troubleshooting

### Common Issues

1. **"Missing workflow id" error**:
   - Check that `NEXT_PUBLIC_CHATKIT_WORKFLOW_ID` is set correctly
   - Ensure the workflow ID starts with `wf_`

2. **"Missing OPENAI_API_KEY" error**:
   - Verify your API key is correct
   - Check that the environment variable is set in Vercel

3. **Build failures**:
   - Check the build logs in Vercel dashboard
   - Ensure all dependencies are in `package.json`

4. **Chat not working**:
   - Verify your OpenAI API key has ChatKit access
   - Check that your workflow is properly configured

### Getting Help

- Check Vercel deployment logs
- Review OpenAI ChatKit documentation
- Open an issue in this repository

## Custom Domain (Optional)

To use a custom domain:

1. Go to "Settings" → "Domains"
2. Add your domain
3. Follow DNS configuration instructions
4. Wait for SSL certificate provisioning

## Performance Optimization

The app is already optimized for Vercel with:
- Edge runtime for API routes
- Automatic static optimization
- Image optimization
- CDN distribution

## Monitoring

Vercel provides built-in monitoring:
- Function execution logs
- Performance metrics
- Error tracking
- Analytics (if enabled)

Ihr Recruiting Agent ist jetzt live! 🚀