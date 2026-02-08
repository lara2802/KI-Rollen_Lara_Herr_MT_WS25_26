# Recruiting Agent - Vercel Deployment Script (PowerShell)

Write-Host "🚀 Deploying Recruiting Agent to Vercel..." -ForegroundColor Green

# Check if Vercel CLI is installed
try {
    $vercelVersion = vercel --version 2>$null
    Write-Host "✅ Vercel CLI is installed: $vercelVersion" -ForegroundColor Green
} catch {
    Write-Host "❌ Vercel CLI is not installed. Installing..." -ForegroundColor Red
    npm install -g vercel
}

# Check if user is logged in to Vercel
try {
    $user = vercel whoami 2>$null
    Write-Host "✅ Logged in as: $user" -ForegroundColor Green
} catch {
    Write-Host "🔐 Please log in to Vercel..." -ForegroundColor Yellow
    vercel login
}

# Deploy to Vercel
Write-Host "📦 Deploying to Vercel..." -ForegroundColor Blue
vercel --prod

Write-Host "✅ Deployment complete!" -ForegroundColor Green
Write-Host ""
Write-Host "📋 Next steps:" -ForegroundColor Cyan
Write-Host "1. Set environment variables in Vercel dashboard:" -ForegroundColor White
Write-Host "   - OPENAI_API_KEY" -ForegroundColor Gray
Write-Host "   - NEXT_PUBLIC_CHATKIT_WORKFLOW_ID" -ForegroundColor Gray
Write-Host "2. Redeploy after setting environment variables" -ForegroundColor White
Write-Host "3. Test your deployment" -ForegroundColor White
Write-Host ""
Write-Host "🔗 Your app will be available at the URL shown above" -ForegroundColor Cyan
