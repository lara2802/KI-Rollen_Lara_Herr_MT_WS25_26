#!/bin/bash

# Recruiting Agent - Vercel Deployment Script

echo "🚀 Deploying Recruiting Agent to Vercel..."

# Check if Vercel CLI is installed
if ! command -v vercel &> /dev/null; then
    echo "❌ Vercel CLI is not installed. Installing..."
    npm install -g vercel
fi

# Check if user is logged in to Vercel
if ! vercel whoami &> /dev/null; then
    echo "🔐 Please log in to Vercel..."
    vercel login
fi

# Deploy to Vercel
echo "📦 Deploying to Vercel..."
vercel --prod

echo "✅ Deployment complete!"
echo ""
echo "📋 Next steps:"
echo "1. Set environment variables in Vercel dashboard:"
echo "   - OPENAI_API_KEY"
echo "   - NEXT_PUBLIC_CHATKIT_WORKFLOW_ID"
echo "2. Redeploy after setting environment variables"
echo "3. Test your deployment"
echo ""
echo "🔗 Your app will be available at the URL shown above"